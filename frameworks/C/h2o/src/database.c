/*
 Copyright (c) 2016 Anton Valentinov Kirilov

 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 associated documentation files (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge, publish, distribute,
 sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all copies or
 substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <h2o.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <postgresql/libpq-fe.h>

#include "database.h"
#include "error.h"
#include "list.h"
#include "thread.h"

#define IS_RESETTING 1
#define IS_WRITING 2

typedef struct {
	list_t l;
	PGconn *conn;
	thread_context_t *ctx;
	db_query_param_t *param;
	list_t *prepared_statement;
	h2o_socket_t *sock;
	uint_fast32_t flags;
	h2o_timeout_entry_t h2o_timeout_entry;
} db_conn_t;

typedef struct {
	list_t l;
	const char *name;
	const char *query;
} prepared_statement_t;

static int do_database_write(db_conn_t *db_conn);
static int do_execute_query(db_conn_t *db_conn, bool direct_notification);
static void error_notification(thread_context_t *ctx, bool timeout, const char *error_string);
static void on_database_connect_error(db_conn_t *db_conn, bool timeout, const char *error_string);
static void on_database_connect_timeout(h2o_timeout_entry_t *entry);
static void on_database_error(db_conn_t *db_conn, const char *error_string);
static void on_database_read_ready(h2o_socket_t *db_sock, const char *err);
static void on_database_timeout(h2o_timeout_entry_t *entry);
static void on_database_write_ready(h2o_socket_t *db_sock, const char *err);
static void poll_database_connection(h2o_socket_t *db_sock, const char *err);
static void process_query(db_conn_t *db_conn);
static void start_database_connect(thread_context_t *ctx, db_conn_t *db_conn);

static int do_database_write(db_conn_t *db_conn)
{
	assert(db_conn->param);

	int ret = db_conn->param->on_write_ready(db_conn->param, db_conn->conn);

	if (!ret)
		db_conn->flags &= ~IS_WRITING;
	else if (ret < 0) {
		ERROR(PQerrorMessage(db_conn->conn));
		on_database_error(db_conn, DB_ERROR);
	}
	else {
		h2o_socket_notify_write(db_conn->sock, on_database_write_ready);
		ret = 0;
	}

	return ret;
}

static int do_execute_query(db_conn_t *db_conn, bool direct_notification)
{
	const int ec = db_conn->param->flags & IS_PREPARED ?
	               PQsendQueryPrepared(db_conn->conn,
	                                   db_conn->param->command,
	                                   db_conn->param->nParams,
	                                   db_conn->param->paramValues,
	                                   db_conn->param->paramLengths,
	                                   db_conn->param->paramFormats,
	                                   db_conn->param->resultFormat) :
	               PQsendQuery(db_conn->conn, db_conn->param->command);
	int ret = 1;

	if (ec) {
		if (db_conn->param->flags & IS_SINGLE_ROW)
			PQsetSingleRowMode(db_conn->conn);

		db_conn->h2o_timeout_entry.cb = on_database_timeout;
		h2o_timeout_link(db_conn->ctx->event_loop.h2o_ctx.loop,
		                 &db_conn->ctx->db_state.h2o_timeout,
		                 &db_conn->h2o_timeout_entry);
		h2o_socket_read_start(db_conn->sock, on_database_read_ready);

		const int send_status = PQflush(db_conn->conn);

		if (send_status < 0) {
			if (direct_notification)
				db_conn->param = NULL;

			LIBRARY_ERROR("PQflush", PQerrorMessage(db_conn->conn));
			on_database_error(db_conn, DB_ERROR);
		}
		else {
			ret = 0;

			if (send_status)
				h2o_socket_notify_write(db_conn->sock, on_database_write_ready);
		}
	}
	else {
		if (direct_notification)
			db_conn->param = NULL;

		ERROR(PQerrorMessage(db_conn->conn));
		on_database_error(db_conn, DB_ERROR);
	}

	return ret;
}

static void error_notification(thread_context_t *ctx, bool timeout, const char *error_string)
{
	if (!--ctx->db_state.db_conn_num) {
		// We don't want to keep requests waiting for an unbounded amount of time.
		list_t *iter = ctx->db_state.queries.head;

		ctx->db_state.queries.head = NULL;
		ctx->db_state.queries.tail = &ctx->db_state.queries.head;
		ctx->db_state.query_num = 0;

		if (iter)
			do {
				db_query_param_t * const param = H2O_STRUCT_FROM_MEMBER(db_query_param_t, l, iter);

				// The callback may free the db_query_param_t structure.
				iter = iter->next;

				if (timeout)
					param->on_timeout(param);
				else
					param->on_error(param, error_string);
			} while (iter);
	}
}

static void on_database_connect_error(db_conn_t *db_conn, bool timeout, const char *error_string)
{
	thread_context_t * const ctx = db_conn->ctx;

	error_notification(ctx, timeout, error_string);
	h2o_timeout_unlink(&db_conn->h2o_timeout_entry);
	h2o_socket_read_stop(db_conn->sock);
	h2o_socket_close(db_conn->sock);
	PQfinish(db_conn->conn);
	free(db_conn);
}

static void on_database_connect_timeout(h2o_timeout_entry_t *entry)
{
	db_conn_t * const db_conn = H2O_STRUCT_FROM_MEMBER(db_conn_t, h2o_timeout_entry, entry);

	ERROR(DB_TIMEOUT_ERROR);
	on_database_connect_error(db_conn, true, DB_TIMEOUT_ERROR);
}

static void on_database_error(db_conn_t *db_conn, const char *error_string)
{
	if (db_conn->prepared_statement)
		on_database_connect_error(db_conn, false, error_string);
	else {
		if (db_conn->param) {
			db_conn->param->on_error(db_conn->param, error_string);
			db_conn->param = NULL;
		}

		if (PQstatus(db_conn->conn) == CONNECTION_OK) {
			h2o_timeout_unlink(&db_conn->h2o_timeout_entry);
			h2o_socket_read_stop(db_conn->sock);
			process_query(db_conn);
		}
		else
			start_database_connect(db_conn->ctx, db_conn);
	}
}

static void on_database_read_ready(h2o_socket_t *db_sock, const char *err)
{
	db_conn_t * const db_conn = db_sock->data;

	if (err)
		ERROR(err);
	else {
		if (PQconsumeInput(db_conn->conn)) {
			const int send_status = PQflush(db_conn->conn);

			if (send_status > 0)
				h2o_socket_notify_write(db_conn->sock, on_database_write_ready);

			if (send_status >= 0) {
				while (!PQisBusy(db_conn->conn)) {
					PGresult * const result = PQgetResult(db_conn->conn);

					if (db_conn->param)
						switch (db_conn->param->on_result(db_conn->param, result)) {
							case WANT_WRITE:
								db_conn->flags |= IS_WRITING;

								if (do_database_write(db_conn))
									return;

								break;
							case DONE:
								db_conn->param = NULL;
								h2o_timeout_unlink(&db_conn->h2o_timeout_entry);
								break;
							default:
								break;
						}
					else if (result) {
						if (PQresultStatus(result) != PGRES_COMMAND_OK)
							LIBRARY_ERROR("PQresultStatus", PQresultErrorMessage(result));

						PQclear(result);
					}

					if (!result) {
						assert(!db_conn->param);
						h2o_timeout_unlink(&db_conn->h2o_timeout_entry);
						h2o_socket_read_stop(db_conn->sock);
						process_query(db_conn);
						break;
					}
				}

				return;
			}
		}

		ERROR(PQerrorMessage(db_conn->conn));
	}

	on_database_error(db_conn, DB_ERROR);
}

static void on_database_timeout(h2o_timeout_entry_t *entry)
{
	db_conn_t * const db_conn = H2O_STRUCT_FROM_MEMBER(db_conn_t, h2o_timeout_entry, entry);

	ERROR(DB_TIMEOUT_ERROR);

	if (db_conn->param) {
		db_conn->param->on_timeout(db_conn->param);
		db_conn->param = NULL;
	}

	start_database_connect(db_conn->ctx, db_conn);
}

static void on_database_write_ready(h2o_socket_t *db_sock, const char *err)
{
	db_conn_t * const db_conn = db_sock->data;

	if (err) {
		ERROR(err);
		on_database_error(db_conn, DB_ERROR);
	}
	else {
		const int send_status = PQflush(db_conn->conn);

		if (!send_status) {
			if (db_conn->flags & IS_WRITING && db_conn->param)
				do_database_write(db_conn);
		}
		else if (send_status < 0) {
			LIBRARY_ERROR("PQflush", PQerrorMessage(db_conn->conn));
			on_database_error(db_conn, DB_ERROR);
		}
		else
			h2o_socket_notify_write(db_conn->sock, on_database_write_ready);
	}
}

static void poll_database_connection(h2o_socket_t *db_sock, const char *err)
{
	db_conn_t * const db_conn = db_sock->data;

	if (err)
		ERROR(err);
	else {
		const PostgresPollingStatusType status = db_conn->flags & IS_RESETTING ?
		                                         PQresetPoll(db_conn->conn) :
		                                         PQconnectPoll(db_conn->conn);

		switch (status) {
			case PGRES_POLLING_WRITING:
				if (!h2o_socket_is_writing(db_conn->sock))
					h2o_socket_notify_write(db_conn->sock, poll_database_connection);

				h2o_socket_read_stop(db_conn->sock);
				return;
			case PGRES_POLLING_OK:
				if (PQsetnonblocking(db_conn->conn, 1)) {
					LIBRARY_ERROR("PQsetnonblocking", PQerrorMessage(db_conn->conn));
					break;
				}

				h2o_timeout_unlink(&db_conn->h2o_timeout_entry);
				h2o_socket_read_stop(db_conn->sock);
				process_query(db_conn);
				return;
			case PGRES_POLLING_READING:
				h2o_socket_read_start(db_conn->sock, poll_database_connection);
				return;
			default:
				ERROR(PQerrorMessage(db_conn->conn));
		}
	}

	on_database_connect_error(db_conn, false, DB_ERROR);
}

static void process_query(db_conn_t *db_conn)
{
	if (db_conn->prepared_statement) {
		const prepared_statement_t * const p = H2O_STRUCT_FROM_MEMBER(prepared_statement_t,
		                                                              l,
		                                                              db_conn->prepared_statement);

		if (PQsendPrepare(db_conn->conn, p->name, p->query, 0, NULL)) {
			db_conn->prepared_statement = p->l.next;
			db_conn->h2o_timeout_entry.cb = on_database_connect_timeout;
			h2o_timeout_link(db_conn->ctx->event_loop.h2o_ctx.loop,
			                 &db_conn->ctx->db_state.h2o_timeout,
			                 &db_conn->h2o_timeout_entry);
			h2o_socket_read_start(db_conn->sock, on_database_read_ready);
			on_database_write_ready(db_conn->sock, NULL);
		}
		else {
			LIBRARY_ERROR("PQsendPrepare", PQerrorMessage(db_conn->conn));
			on_database_connect_error(db_conn, false, DB_ERROR);
		}
	}
	else if (db_conn->ctx->db_state.query_num) {
		db_conn->ctx->db_state.query_num--;

		if (db_conn->ctx->db_state.queries.tail == &db_conn->ctx->db_state.queries.head->next) {
			assert(!db_conn->ctx->db_state.query_num);
			db_conn->ctx->db_state.queries.tail = &db_conn->ctx->db_state.queries.head;
		}

		db_conn->param = H2O_STRUCT_FROM_MEMBER(db_query_param_t,
		                                        l,
		                                        db_conn->ctx->db_state.queries.head);
		db_conn->ctx->db_state.queries.head = db_conn->ctx->db_state.queries.head->next;
		do_execute_query(db_conn, false);
	}
	else {
		db_conn->l.next = db_conn->ctx->db_state.db_conn;
		db_conn->ctx->db_state.db_conn = &db_conn->l;
		db_conn->ctx->db_state.free_db_conn_num++;
	}
}

static void start_database_connect(thread_context_t *ctx, db_conn_t *db_conn)
{
	if (db_conn) {
		db_conn->flags = IS_RESETTING;
		h2o_timeout_unlink(&db_conn->h2o_timeout_entry);
		h2o_socket_read_stop(db_conn->sock);
		h2o_socket_close(db_conn->sock);

		if (!PQresetStart(db_conn->conn)) {
			LIBRARY_ERROR("PQresetStart", PQerrorMessage(db_conn->conn));
			goto error_dup;
		}
	}
	else {
		ctx->db_state.db_conn_num++;
		db_conn = h2o_mem_alloc(sizeof(*db_conn));
		memset(db_conn, 0, sizeof(*db_conn));

		const char * const conninfo = ctx->config->db_host ? ctx->config->db_host : "";

		db_conn->conn = PQconnectStart(conninfo);

		if (!db_conn->conn) {
			errno = ENOMEM;
			STANDARD_ERROR("PQconnectStart");
			goto error_connect;
		}

		if (PQstatus(db_conn->conn) == CONNECTION_BAD) {
			LIBRARY_ERROR("PQstatus", PQerrorMessage(db_conn->conn));
			goto error_dup;
		}
	}

	const int sd = dup(PQsocket(db_conn->conn));

	if (sd < 0) {
		STANDARD_ERROR("dup");
		goto error_dup;
	}

	const int flags = fcntl(sd, F_GETFD);

	if (flags < 0 || fcntl(sd, F_SETFD, flags | FD_CLOEXEC)) {
		STANDARD_ERROR("fcntl");
		goto error_fcntl;
	}

	db_conn->sock = h2o_evloop_socket_create(ctx->event_loop.h2o_ctx.loop,
	                                         sd,
	                                         H2O_SOCKET_FLAG_DONT_READ);

	if (db_conn->sock) {
		db_conn->sock->data = db_conn;
		db_conn->ctx = ctx;
		db_conn->h2o_timeout_entry.cb = on_database_connect_timeout;
		db_conn->prepared_statement = ctx->global_data->prepared_statements;
		h2o_timeout_link(ctx->event_loop.h2o_ctx.loop,
		                 &ctx->db_state.h2o_timeout,
		                 &db_conn->h2o_timeout_entry);
		h2o_socket_notify_write(db_conn->sock, poll_database_connection);
		return;
	}

	errno = ENOMEM;
	STANDARD_ERROR("h2o_evloop_socket_create");
error_fcntl:
	close(sd);
error_dup:
	PQfinish(db_conn->conn);
error_connect:
	free(db_conn);
	error_notification(ctx, false, DB_ERROR);
}

void add_prepared_statement(const char *name, const char *query, list_t **prepared_statements)
{
	prepared_statement_t * const p = h2o_mem_alloc(sizeof(*p));

	memset(p, 0, sizeof(*p));
	p->l.next = *prepared_statements;
	p->name = name;
	p->query = query;
	*prepared_statements = &p->l;
}

int execute_query(thread_context_t *ctx, db_query_param_t *param)
{
	int ret = 1;

	if (ctx->db_state.free_db_conn_num) {
		db_conn_t * const db_conn = H2O_STRUCT_FROM_MEMBER(db_conn_t, l, ctx->db_state.db_conn);

		ctx->db_state.db_conn = db_conn->l.next;
		ctx->db_state.free_db_conn_num--;
		db_conn->param = param;
		ret = do_execute_query(db_conn, true);
	}
	else if (ctx->db_state.query_num < ctx->config->max_query_num) {
		if (ctx->db_state.db_conn_num < ctx->config->max_db_conn_num)
			start_database_connect(ctx, NULL);

		if (ctx->db_state.db_conn_num) {
			param->l.next = NULL;
			*ctx->db_state.queries.tail = &param->l;
			ctx->db_state.queries.tail = &param->l.next;
			ctx->db_state.query_num++;
			ret = 0;
		}
	}

	return ret;
}

void free_database_state(h2o_loop_t *loop, db_state_t *db_state)
{
	assert(!db_state->query_num && db_state->free_db_conn_num == db_state->db_conn_num);

	list_t *iter = db_state->db_conn;

	if (iter)
		do {
			db_conn_t * const db_conn = H2O_STRUCT_FROM_MEMBER(db_conn_t, l, iter);

			iter = iter->next;
			assert(!db_conn->param && !h2o_timeout_is_linked(&db_conn->h2o_timeout_entry));
			h2o_socket_close(db_conn->sock);
			PQfinish(db_conn->conn);
			free(db_conn);
		} while (iter);

	h2o_timeout_dispose(loop, &db_state->h2o_timeout);
}

void initialize_database_state(h2o_loop_t *loop, db_state_t *db_state)
{
	memset(db_state, 0, sizeof(*db_state));
	db_state->queries.tail = &db_state->queries.head;
	h2o_timeout_init(loop, &db_state->h2o_timeout, H2O_DEFAULT_HTTP1_REQ_TIMEOUT);
}

void remove_prepared_statements(list_t *prepared_statements)
{
	if (prepared_statements)
		do {
			prepared_statement_t * const p = H2O_STRUCT_FROM_MEMBER(prepared_statement_t,
			                                                        l,
			                                                        prepared_statements);

			prepared_statements = prepared_statements->next;
			free(p);
		} while (prepared_statements);
}
