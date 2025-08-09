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
#include "event_loop.h"
#include "global_data.h"
#include "list.h"

#define MS_IN_S 1000

// Database connection state
#define EXPECT_SYNC 1
#define IGNORE_RESULT 2
#define RESET 4

typedef struct {
	list_t l;
	PGconn *conn;
	db_conn_pool_t *pool;
	const list_t *prepared_statement;
	queue_t queries;
	h2o_socket_t *sock;
	size_t query_num;
	uint_fast32_t flags;
	int sd;
	h2o_timer_t timer;
} db_conn_t;

typedef struct {
	list_t l;
	char *name;
	char *query;
} prepared_statement_t;

static h2o_socket_t *create_socket(int sd, h2o_loop_t *loop);
static int do_execute_query(db_conn_t *conn, db_query_param_t *param);
static void error_notification(db_conn_pool_t *pool, bool timeout, const char *error_string);
static int flush_connection(h2o_socket_cb cb, db_conn_t *conn);
static void on_database_connect_error(db_conn_t *conn, bool timeout, const char *error_string);
static void on_database_connect_read_ready(h2o_socket_t *sock, const char *err);
static void on_database_connect_timeout(h2o_timer_t *timer);
static void on_database_connect_write_ready(h2o_socket_t *sock, const char *err);
static void on_database_error(db_conn_t *conn, const char *error_string);
static void on_database_read_ready(h2o_socket_t *sock, const char *err);
static void on_database_timeout(h2o_timer_t *timer);
static void on_database_write_ready(h2o_socket_t *sock, const char *err);
static void on_process_queries(void *arg);
static void poll_database_connection(h2o_socket_t *sock, const char *err);
static void prepare_statements(db_conn_t *conn);
static void process_queries(db_conn_pool_t *pool);
static void remove_connection(db_conn_t *conn);
static void start_database_connect(db_conn_pool_t *pool, db_conn_t *conn);

static h2o_socket_t *create_socket(int sd, h2o_loop_t *loop)
{
	sd = dup(sd);

	if (sd < 0) {
		STANDARD_ERROR("dup");
		return NULL;
	}

	const int flags = fcntl(sd, F_GETFD);

	if (flags < 0 || fcntl(sd, F_SETFD, flags | FD_CLOEXEC)) {
		STANDARD_ERROR("fcntl");
		close(sd);
		return NULL;
	}

	h2o_socket_t * const ret = h2o_evloop_socket_create(loop, sd, H2O_SOCKET_FLAG_DONT_READ);

	if (!ret) {
		errno = ENOMEM;
		STANDARD_ERROR("h2o_evloop_socket_create");
		close(sd);
	}

	return ret;
}

static int do_execute_query(db_conn_t *conn, db_query_param_t *param)
{
	assert(conn->query_num);
	assert((conn->queries.head && conn->query_num < conn->pool->config->max_pipeline_query_num) ||
	       (!conn->queries.head && conn->query_num == conn->pool->config->max_pipeline_query_num));

	const int ec = param->flags & IS_PREPARED ?
	               PQsendQueryPrepared(conn->conn,
	                                   param->command,
	                                   param->nParams,
	                                   param->paramValues,
	                                   param->paramLengths,
	                                   param->paramFormats,
	                                   param->resultFormat) :
	               PQsendQueryParams(conn->conn,
	                                 param->command,
	                                 param->nParams,
	                                 param->paramTypes,
	                                 param->paramValues,
	                                 param->paramLengths,
	                                 param->paramFormats,
	                                 param->resultFormat);

	if (!ec) {
		ERROR(PQerrorMessage(conn->conn));
		return 1;
	}

	if (!PQsendPipelineSync(conn->conn)) {
		LIBRARY_ERROR("PQsendPipelineSync", PQerrorMessage(conn->conn));
		return 1;
	}

	if (!conn->queries.head && !(conn->flags & (EXPECT_SYNC | IGNORE_RESULT))) {
		assert(!h2o_timer_is_linked(&conn->timer));
		conn->timer.cb = on_database_timeout;
		h2o_timer_link(conn->pool->loop, conn->pool->config->db_timeout * MS_IN_S, &conn->timer);
	}

	param->l.next = NULL;
	*conn->queries.tail = &param->l;
	conn->queries.tail = &param->l.next;
	conn->query_num--;
	return 0;
}

static void error_notification(db_conn_pool_t *pool, bool timeout, const char *error_string)
{
	assert(pool->conn_num < pool->config->max_db_conn_num);

	if (++pool->conn_num == pool->config->max_db_conn_num) {
		// We don't want to keep requests waiting for an unbounded amount of time.
		list_t *iter = pool->queries.head;

		pool->queries.head = NULL;
		pool->queries.tail = &pool->queries.head;
		pool->query_num = pool->config->max_query_num;

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

static int flush_connection(h2o_socket_cb cb, db_conn_t *conn)
{
	const int send_status = PQflush(conn->conn);

	if (send_status < 0)
		LIBRARY_ERROR("PQflush", PQerrorMessage(conn->conn));
	else if (send_status && !h2o_socket_is_writing(conn->sock))
		h2o_socket_notify_write(conn->sock, cb);

	return send_status < 0;
}

static void on_database_connect_error(db_conn_t *conn, bool timeout, const char *error_string)
{
	db_conn_pool_t * const pool = conn->pool;

	h2o_timer_unlink(&conn->timer);
	h2o_socket_read_stop(conn->sock);
	h2o_socket_close(conn->sock);
	PQfinish(conn->conn);
	free(conn);
	error_notification(pool, timeout, error_string);
}

static void on_database_connect_read_ready(h2o_socket_t *sock, const char *err)
{
	db_conn_t * const conn = sock->data;

	if (err) {
		ERROR(err);
		on_database_connect_error(conn, false, DB_ERROR);
		return;
	}

	if (!PQconsumeInput(conn->conn)) {
		LIBRARY_ERROR("PQconsumeInput", PQerrorMessage(conn->conn));
		on_database_connect_error(conn, false, DB_ERROR);
		return;
	}

	if (flush_connection(on_database_connect_write_ready, conn)) {
		on_database_connect_error(conn, false, DB_ERROR);
		return;
	}

	while (!PQisBusy(conn->conn)) {
		PGresult * const result = PQgetResult(conn->conn);

		if (result) {
			switch (PQresultStatus(result)) {
				case PGRES_COMMAND_OK:
					break;
				case PGRES_PIPELINE_SYNC:
					PQclear(result);
					h2o_timer_unlink(&conn->timer);
					h2o_socket_read_stop(conn->sock);
					h2o_socket_read_start(conn->sock, on_database_read_ready);
					*conn->pool->conn.tail = &conn->l;
					conn->pool->conn.tail = &conn->l.next;
					conn->l.next = NULL;
					process_queries(conn->pool);
					return;
				default:
					LIBRARY_ERROR("PQresultStatus", PQresultErrorMessage(result));
					PQclear(result);
					on_database_connect_error(conn, false, DB_ERROR);
					return;
			}

			PQclear(result);
		}
	}
}

static void on_database_connect_timeout(h2o_timer_t *timer)
{
	db_conn_t * const conn = H2O_STRUCT_FROM_MEMBER(db_conn_t, timer, timer);

	ERROR(DB_TIMEOUT_ERROR);
	on_database_connect_error(conn, true, DB_TIMEOUT_ERROR);
}

static void on_database_connect_write_ready(h2o_socket_t *sock, const char *err)
{
	db_conn_t * const conn = sock->data;

	if (err) {
		ERROR(err);
		on_database_connect_error(conn, false, err);
	}
	else if (flush_connection(on_database_connect_write_ready, conn))
		on_database_connect_error(conn, false, DB_ERROR);
}

static void on_database_error(db_conn_t *conn, const char *error_string)
{
	remove_connection(conn);

	if (conn->queries.head)
		do {
			db_query_param_t * const param = H2O_STRUCT_FROM_MEMBER(db_query_param_t,
			                                                        l,
			                                                        conn->queries.head);

			// The callback may free the db_query_param_t structure.
			conn->queries.head = param->l.next;
			param->on_error(param, error_string);
		} while (conn->queries.head);

	start_database_connect(conn->pool, conn);
}

static void on_database_read_ready(h2o_socket_t *sock, const char *err)
{
	db_conn_t * const conn = sock->data;

	if (err) {
		ERROR(err);
		on_database_error(conn, err);
		return;
	}

	if (!PQconsumeInput(conn->conn)) {
		LIBRARY_ERROR("PQconsumeInput", PQerrorMessage(conn->conn));
		on_database_error(conn, DB_ERROR);
		return;
	}

	if (flush_connection(on_database_write_ready, conn)) {
		on_database_error(conn, DB_ERROR);
		return;
	}

	const bool removed = !conn->query_num;

	while (!PQisBusy(conn->conn)) {
		PGresult * const result = PQgetResult(conn->conn);

		if (conn->flags & IGNORE_RESULT) {
			if (result)
				PQclear(result);
			else
				conn->flags &= ~IGNORE_RESULT;
		}
		else if (conn->flags & EXPECT_SYNC) {
			if (PQresultStatus(result) == PGRES_PIPELINE_SYNC) {
				PQclear(result);
				conn->flags &= ~EXPECT_SYNC;
			}
			else {
				LIBRARY_ERROR("PQresultStatus", PQresultErrorMessage(result));
				PQclear(result);
				on_database_error(conn, DB_ERROR);
				return;
			}
		}
		else if (conn->queries.head) {
			db_query_param_t * const param = H2O_STRUCT_FROM_MEMBER(db_query_param_t,
			                                                        l,
			                                                        conn->queries.head);
			// The callback may free the db_query_param_t structure.
			list_t * const next = param->l.next;
			const bool nonnull_result = !!result;

			if (param->on_result(param, result) == DONE) {
				conn->query_num++;
				h2o_timer_unlink(&conn->timer);
				conn->timer.cb = on_database_timeout;
				h2o_timer_link(conn->pool->loop,
				               conn->pool->config->db_timeout * MS_IN_S,
				               &conn->timer);
				conn->flags |= EXPECT_SYNC;
				conn->queries.head = next;

				if (!next)
					conn->queries.tail = &conn->queries.head;

				if (nonnull_result)
					conn->flags |= IGNORE_RESULT;
			}
			else
				assert(nonnull_result);
		}
		else {
			assert(!result);
			h2o_timer_unlink(&conn->timer);
			break;
		}
	}

	for (PGnotify *notify = PQnotifies(conn->conn); notify; notify = PQnotifies(conn->conn))
		PQfreemem(notify);

	if (removed && conn->query_num) {
		*conn->pool->conn.tail = &conn->l;
		conn->pool->conn.tail = &conn->l.next;
		conn->l.next = NULL;
	}

	process_queries(conn->pool);
}

static void on_database_timeout(h2o_timer_t *timer)
{
	db_conn_t * const conn = H2O_STRUCT_FROM_MEMBER(db_conn_t, timer, timer);

	ERROR(DB_TIMEOUT_ERROR);

	if (conn->queries.head) {
		db_query_param_t * const param = H2O_STRUCT_FROM_MEMBER(db_query_param_t,
		                                                        l,
		                                                        conn->queries.head);

		conn->queries.head = param->l.next;
		param->on_timeout(param);
	}

	on_database_error(conn, DB_TIMEOUT_ERROR);
}

static void on_database_write_ready(h2o_socket_t *sock, const char *err)
{
	db_conn_t * const conn = sock->data;

	if (err) {
		ERROR(err);
		on_database_error(conn, err);
	}
	else if (flush_connection(on_database_write_ready, conn))
		on_database_error(conn, DB_ERROR);
}

static void on_process_queries(void *arg)
{
	list_t *iter = NULL;
	db_conn_pool_t * const pool = arg;
	size_t query_num = 0;

	while (pool->queries.head && pool->conn.head) {
		db_conn_t * const conn = H2O_STRUCT_FROM_MEMBER(db_conn_t, l, pool->conn.head);
		db_query_param_t * const param = H2O_STRUCT_FROM_MEMBER(db_query_param_t,
		                                                        l,
		                                                        conn->pool->queries.head);

		assert(conn->query_num);
		assert(pool->query_num < pool->config->max_query_num);
		pool->conn.head = conn->l.next;
		pool->queries.head = param->l.next;

		if (!pool->conn.head) {
			assert(pool->conn.tail == &conn->l.next);
			pool->conn.tail = &pool->conn.head;
		}

		if (++pool->query_num == pool->config->max_query_num) {
			assert(!pool->queries.head);
			assert(pool->queries.tail == &param->l.next);
			pool->queries.tail = &pool->queries.head;
		}

		if (do_execute_query(conn, param)) {
			param->on_error(param, DB_ERROR);
			on_database_error(conn, DB_ERROR);
		}
		else {
			query_num++;

			if (conn->query_num) {
				*pool->conn.tail = &conn->l;
				pool->conn.tail = &conn->l.next;
				conn->l.next = NULL;
			}
			else {
				conn->l.next = iter;
				iter = &conn->l;
			}
		}
	}

	if (iter)
		do {
			db_conn_t * const conn = H2O_STRUCT_FROM_MEMBER(db_conn_t, l, iter);

			iter = conn->l.next;

			if (flush_connection(on_database_write_ready, conn))
				on_database_error(conn, DB_ERROR);
		} while (iter);

	pool->conn.tail = &pool->conn.head;
	pool->process_queries = false;
	query_num += pool->config->max_query_num - pool->query_num;

	for (iter = pool->conn.head; iter;) {
		db_conn_t * const conn = H2O_STRUCT_FROM_MEMBER(db_conn_t, l, iter);

		iter = conn->l.next;

		if (flush_connection(on_database_write_ready, conn)) {
			*pool->conn.tail = iter;
			on_database_error(conn, DB_ERROR);
		}
		else
			pool->conn.tail = &conn->l.next;
	}

	const size_t conn_num = pool->config->max_db_conn_num - pool->conn_num;

	if (query_num > conn_num)
		for (query_num -= conn_num; pool->conn_num && query_num; query_num--)
			start_database_connect(pool, NULL);
}

static void poll_database_connection(h2o_socket_t *sock, const char *err)
{
	db_conn_t * const conn = sock->data;

	if (err)
		ERROR(err);
	else {
		const PostgresPollingStatusType status = conn->flags & RESET ?
		                                         PQresetPoll(conn->conn) :
		                                         PQconnectPoll(conn->conn);
		const int sd = PQsocket(conn->conn);

		switch (status) {
			case PGRES_POLLING_WRITING:
				h2o_socket_read_stop(conn->sock);

				if (sd != conn->sd) {
					h2o_socket_t * const sock = create_socket(sd, conn->pool->loop);

					if (!sock)
						break;

					h2o_socket_close(conn->sock);
					conn->sd = sd;
					conn->sock = sock;
				}

				if (!h2o_socket_is_writing(conn->sock))
					h2o_socket_notify_write(conn->sock, poll_database_connection);

				return;
			case PGRES_POLLING_OK:
				h2o_timer_unlink(&conn->timer);
				h2o_socket_read_stop(conn->sock);

				if (PQsetnonblocking(conn->conn, 1)) {
					LIBRARY_ERROR("PQsetnonblocking", PQerrorMessage(conn->conn));
					break;
				}

				if (!PQenterPipelineMode(conn->conn)) {
					ERROR("PQenterPipelineMode");
					break;
				}

				if (sd != conn->sd) {
					h2o_socket_t * const sock = create_socket(sd, conn->pool->loop);

					if (!sock)
						break;

					h2o_socket_close(conn->sock);
					conn->sd = sd;
					conn->sock = sock;
				}

				conn->flags &= ~RESET;
				prepare_statements(conn);
				return;
			case PGRES_POLLING_READING:
				if (sd != conn->sd) {
					h2o_socket_t * const sock = create_socket(sd, conn->pool->loop);

					if (!sock)
						break;

					h2o_socket_read_stop(conn->sock);
					h2o_socket_close(conn->sock);
					conn->sd = sd;
					conn->sock = sock;
				}

				h2o_socket_read_start(conn->sock, poll_database_connection);
				return;
			default:
				ERROR(PQerrorMessage(conn->conn));
		}
	}

	on_database_connect_error(conn, false, DB_ERROR);
}

static void prepare_statements(db_conn_t *conn)
{
	if (conn->prepared_statement) {
		const list_t *iter = conn->prepared_statement;

		do {
			const prepared_statement_t * const p = H2O_STRUCT_FROM_MEMBER(prepared_statement_t,
			                                                              l,
			                                                              iter);

			if (!PQsendPrepare(conn->conn, p->name, p->query, 0, NULL)) {
				LIBRARY_ERROR("PQsendPrepare", PQerrorMessage(conn->conn));
				on_database_connect_error(conn, false, DB_ERROR);
				return;
			}

			iter = iter->next;
		} while (iter);

		if (!PQsendPipelineSync(conn->conn)) {
			LIBRARY_ERROR("PQsendPipelineSync", PQerrorMessage(conn->conn));
			on_database_connect_error(conn, false, DB_ERROR);
			return;
		}

		conn->prepared_statement = NULL;
		conn->timer.cb = on_database_connect_timeout;
		h2o_timer_link(conn->pool->loop, conn->pool->config->db_timeout * MS_IN_S, &conn->timer);
		h2o_socket_read_start(conn->sock, on_database_connect_read_ready);
		on_database_connect_write_ready(conn->sock, NULL);
	}
	else {
		h2o_socket_read_start(conn->sock, on_database_read_ready);
		*conn->pool->conn.tail = &conn->l;
		conn->pool->conn.tail = &conn->l.next;
		conn->l.next = NULL;
		process_queries(conn->pool);
	}
}

static void process_queries(db_conn_pool_t *pool)
{
	if (!pool->process_queries && pool->queries.head) {
		task_message_t * const msg = h2o_mem_alloc(sizeof(*msg));

		assert(pool->query_num < pool->config->max_query_num);
		memset(msg, 0, sizeof(*msg));
		msg->arg = pool;
		msg->super.type = TASK;
		msg->task = on_process_queries;
		pool->process_queries = true;
		send_local_message(&msg->super, pool->local_messages);
	}
}

static void remove_connection(db_conn_t *conn)
{
	list_t *iter = conn->pool->conn.head;
	list_t **prev = &conn->pool->conn.head;

	for (; iter && iter != &conn->l; iter = iter->next)
		prev = &iter->next;

	if (iter) {
		*prev = iter->next;

		if (!conn->pool->conn.head) {
			assert(conn->pool->conn.tail == &iter->next);
			conn->pool->conn.tail = &conn->pool->conn.head;
		}
	}
}

static void start_database_connect(db_conn_pool_t *pool, db_conn_t *conn)
{
	if (conn) {
		PGconn * const c = conn->conn;

		h2o_timer_unlink(&conn->timer);
		h2o_socket_read_stop(conn->sock);
		h2o_socket_close(conn->sock);

		if (!PQresetStart(c)) {
			LIBRARY_ERROR("PQresetStart", PQerrorMessage(c));
			goto error_dup;
		}

		memset(conn, 0, sizeof(*conn));
		conn->conn = c;
		conn->flags = RESET;
	}
	else {
		assert(pool->conn_num);
		pool->conn_num--;
		conn = h2o_mem_alloc(sizeof(*conn));
		memset(conn, 0, sizeof(*conn));
		conn->conn = PQconnectStart(pool->conninfo);

		if (!conn->conn) {
			errno = ENOMEM;
			STANDARD_ERROR("PQconnectStart");
			goto error_connect;
		}

		if (PQstatus(conn->conn) == CONNECTION_BAD) {
			LIBRARY_ERROR("PQstatus", PQerrorMessage(conn->conn));
			goto error_dup;
		}
	}

	conn->sd = PQsocket(conn->conn);
	conn->sock = create_socket(conn->sd, pool->loop);

	if (conn->sock) {
		conn->sock->data = conn;
		conn->pool = pool;
		conn->prepared_statement = pool->prepared_statements;
		conn->queries.tail = &conn->queries.head;
		conn->query_num = pool->config->max_pipeline_query_num;
		conn->timer.cb = on_database_connect_timeout;
		h2o_timer_link(pool->loop, pool->config->db_timeout * MS_IN_S, &conn->timer);
		h2o_socket_notify_write(conn->sock, poll_database_connection);
		return;
	}

error_dup:
	PQfinish(conn->conn);
error_connect:
	free(conn);
	error_notification(pool, false, DB_ERROR);
}

void add_prepared_statement(const char *name, const char *query, list_t **prepared_statements)
{
	prepared_statement_t * const p = h2o_mem_alloc(sizeof(*p));

	memset(p, 0, sizeof(*p));
	p->l.next = *prepared_statements;
	p->name = h2o_strdup(NULL, name, SIZE_MAX).base;
	p->query = h2o_strdup(NULL, query, SIZE_MAX).base;
	*prepared_statements = &p->l;
}

int execute_database_query(db_conn_pool_t *pool, db_query_param_t *param)
{
	int ret = 1;

	if (pool->query_num) {
		// Delay sending the database queries to the server, so that if there is a rapid
		// succession of calls to this function, all resultant queries would be inserted
		// into a command pipeline with a smaller number of system calls.
		param->l.next = NULL;
		*pool->queries.tail = &param->l;
		pool->queries.tail = &param->l.next;
		pool->query_num--;
		process_queries(pool);
		ret = 0;
	}

	return ret;
}

void free_database_connection_pool(db_conn_pool_t *pool)
{
	assert(!pool->queries.head);
	assert(pool->query_num == pool->config->max_query_num);

	size_t num = 0;

	if (pool->conn.head)
		do {
			db_conn_t * const conn = H2O_STRUCT_FROM_MEMBER(db_conn_t, l, pool->conn.head);

			assert(!conn->queries.head);
			assert(conn->query_num == pool->config->max_pipeline_query_num);
			assert(!(conn->flags & (EXPECT_SYNC | IGNORE_RESULT | RESET)));
			assert(!h2o_timer_is_linked(&conn->timer));
			h2o_socket_read_stop(conn->sock);
			h2o_socket_close(conn->sock);
			PQfinish(conn->conn);
			pool->conn.head = conn->l.next;
			num++;
			free(conn);
		} while (pool->conn.head);

	assert(num + pool->conn_num == pool->config->max_db_conn_num);
}

void initialize_database_connection_pool(const char *conninfo,
                                         const struct config_t *config,
                                         const list_t *prepared_statements,
                                         h2o_loop_t *loop,
                                         h2o_linklist_t *local_messages,
                                         db_conn_pool_t *pool)
{
	memset(pool, 0, sizeof(*pool));
	pool->config = config;
	pool->conn.tail = &pool->conn.head;
	pool->conninfo = conninfo ? conninfo : "";
	pool->local_messages = local_messages;
	pool->loop = loop;
	pool->prepared_statements = prepared_statements;
	pool->queries.tail = &pool->queries.head;
	pool->conn_num = config->max_db_conn_num;
	pool->query_num = config->max_query_num;
}

void remove_prepared_statements(list_t *prepared_statements)
{
	if (prepared_statements)
		do {
			prepared_statement_t * const p = H2O_STRUCT_FROM_MEMBER(prepared_statement_t,
			                                                        l,
			                                                        prepared_statements);

			prepared_statements = prepared_statements->next;
			free(p->name);
			free(p->query);
			free(p);
		} while (prepared_statements);
}
