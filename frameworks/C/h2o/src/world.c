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
#include <h2o.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <arpa/inet.h>
#include <postgresql/libpq-fe.h>
#include <yajl/yajl_gen.h>

#include "bitset.h"
#include "database.h"
#include "error.h"
#include "request_handler.h"
#include "thread.h"
#include "utility.h"
#include "world.h"

#define ID_KEY "id"
#define MAX_QUERIES 500
#define QUERIES_PARAMETER "queries="
#define RANDOM_NUM_KEY "randomNumber"

typedef struct multiple_query_ctx_t multiple_query_ctx_t;
typedef struct update_ctx_t update_ctx_t;

typedef struct {
	uint32_t id;
	uint32_t random_number;
} query_result_t;

typedef struct {
	multiple_query_ctx_t *ctx;
	const char *id_pointer;
	uint32_t id;
	int id_format;
	int id_len;
	db_query_param_t param;
} query_param_t;

typedef struct {
	const char *id_pointer;
	h2o_req_t *req;
	uint32_t id;
	int id_format;
	int id_len;
	db_query_param_t param;
} single_query_ctx_t;

struct multiple_query_ctx_t {
	yajl_gen gen; // also an error flag
	h2o_req_t *req;
	query_param_t *query_param;
	size_t num_query;
	size_t num_query_in_progress;
	size_t num_result;
	query_result_t res[];
};

typedef struct {
	char *command;
	update_ctx_t *ctx;
	uint32_t random_number;
	bool update;
	db_query_param_t param;
} update_param_t;

struct update_ctx_t {
	yajl_gen gen; // also an error flag
	h2o_req_t *req;
	update_param_t *update_param;
	size_t num_query;
	size_t num_query_in_progress;
	size_t num_result;
	query_result_t res[];
};

static void cleanup_multiple_query(void *data);
static void cleanup_update(void *data);
static size_t get_query_number(h2o_req_t *req);
static void initialize_ids(size_t num_query, query_result_t *res, unsigned int *seed);
static void on_multiple_query_error(db_query_param_t *param, const char *error_string);
static result_return_t on_multiple_query_result(db_query_param_t *param, PGresult *result);
static void on_multiple_query_timeout(db_query_param_t *param);
static void on_single_query_error(db_query_param_t *param, const char *error_string);
static result_return_t on_single_query_result(db_query_param_t *param, PGresult *result);
static void on_single_query_timeout(db_query_param_t *param);
static void on_update_error(db_query_param_t *param, const char *error_string);
static result_return_t on_update_result(db_query_param_t *param, PGresult *result);
static void on_update_timeout(db_query_param_t *param);
static void process_result(PGresult *result, query_result_t *out);
static int serialize_item(uint32_t id, uint32_t random_number, yajl_gen gen);
static void serialize_items(const query_result_t *res,
                            size_t num_result,
                            yajl_gen gen,
                            h2o_req_t *req);

static void cleanup_multiple_query(void *data)
{
	const multiple_query_ctx_t * const query_ctx = data;

	if (query_ctx->gen)
		yajl_gen_free(query_ctx->gen);
}

static void cleanup_update(void *data)
{
	const update_ctx_t * const update_ctx = data;

	if (update_ctx->gen)
		yajl_gen_free(update_ctx->gen);
}

static size_t get_query_number(h2o_req_t *req)
{
	int num_query = 0;

	if (req->query_at < SIZE_MAX) {
		const char * const n = get_query_param(req->path.base + req->query_at + 1,
		                                       req->path.len - req->query_at - 1,
		                                       QUERIES_PARAMETER,
		                                       sizeof(QUERIES_PARAMETER) - 1);

		if (n)
			num_query = atoi(n);
	}

	if (num_query < 1)
		num_query = 1;
	else if (num_query > MAX_QUERIES)
		num_query = MAX_QUERIES;

	return num_query;
}

static void initialize_ids(size_t num_query, query_result_t *res, unsigned int *seed)
{
	// MAX_ID is a relatively small number, so allocate on the stack.
	DEFINE_BITSET(bitset, MAX_ID);

	size_t max_rand = MAX_ID - num_query + 1;

	for (size_t i = 0; i < num_query; i++) {
		res[i].id = get_random_number(max_rand, seed);

		if (BITSET_ISSET(res[i].id, bitset))
			res[i].id = max_rand - 1;

		BITSET_SET(res[i].id++, bitset);
		max_rand++;
	}
}

static void on_multiple_query_error(db_query_param_t *param, const char *error_string)
{
	const query_param_t * const query_param = H2O_STRUCT_FROM_MEMBER(query_param_t,
	                                                                 param,
	                                                                 param);
	multiple_query_ctx_t * const query_ctx = query_param->ctx;

	if (query_ctx->gen) {
		yajl_gen_free(query_ctx->gen);
		query_ctx->gen = NULL;
		send_error(BAD_GATEWAY, error_string, query_ctx->req);
	}

	query_ctx->num_query_in_progress--;
	h2o_mem_release_shared(query_ctx);
}

static result_return_t on_multiple_query_result(db_query_param_t *param, PGresult *result)
{
	query_param_t * const query_param = H2O_STRUCT_FROM_MEMBER(query_param_t,
	                                                           param,
	                                                           param);
	multiple_query_ctx_t * const query_ctx = query_param->ctx;

	if (query_ctx->gen && PQresultStatus(result) == PGRES_TUPLES_OK) {
		thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
		                                                      event_loop.h2o_ctx,
		                                                      query_ctx->req->conn->ctx);

		process_result(result, query_ctx->res + query_ctx->num_result);
		query_ctx->num_query_in_progress--;
		query_ctx->num_result++;

		const size_t num_query_remaining = query_ctx->num_query - query_ctx->num_result;

		if (query_ctx->num_query_in_progress < num_query_remaining) {
			const size_t idx = query_ctx->num_result + query_ctx->num_query_in_progress;

			query_param->id = htonl(query_ctx->res[idx].id);

			if (!execute_query(ctx, &query_param->param)) {
				query_ctx->num_query_in_progress++;
				PQclear(result);
				return DONE;
			}

			yajl_gen_free(query_ctx->gen);
			query_ctx->gen = NULL;
			send_service_unavailable_error(DB_REQ_ERROR, query_ctx->req);
		}
		else if (query_ctx->num_result == query_ctx->num_query)
			serialize_items(query_ctx->res,
			                query_ctx->num_result,
			                query_ctx->gen,
			                query_ctx->req);

		h2o_mem_release_shared(query_ctx);
	}
	else
		on_multiple_query_error(param, PQresultErrorMessage(result));

	PQclear(result);
	return DONE;
}

static void on_multiple_query_timeout(db_query_param_t *param)
{
	const query_param_t * const query_param = H2O_STRUCT_FROM_MEMBER(query_param_t,
	                                                                 param,
	                                                                 param);
	multiple_query_ctx_t * const query_ctx = query_param->ctx;

	if (query_ctx->gen) {
		yajl_gen_free(query_ctx->gen);
		query_ctx->gen = NULL;
		send_error(GATEWAY_TIMEOUT, DB_TIMEOUT_ERROR, query_ctx->req);
	}

	query_ctx->num_query_in_progress--;
	h2o_mem_release_shared(query_ctx);
}

static void on_single_query_error(db_query_param_t *param, const char *error_string)
{
	single_query_ctx_t * const query_ctx = H2O_STRUCT_FROM_MEMBER(single_query_ctx_t,
	                                                              param,
	                                                              param);

	send_error(BAD_GATEWAY, error_string, query_ctx->req);
}

static result_return_t on_single_query_result(db_query_param_t *param, PGresult *result)
{
	single_query_ctx_t * const query_ctx = H2O_STRUCT_FROM_MEMBER(single_query_ctx_t,
	                                                              param,
	                                                              param);

	if (PQresultStatus(result) == PGRES_TUPLES_OK) {
		uint32_t random_number;

		assert(PQnfields(result) == 2);
		assert(PQntuples(result) == 1);
		assert(PQgetlength(result, 0, 1) == sizeof(random_number));

		// Use memcpy() in case the result is not aligned.
		memcpy(&random_number, PQgetvalue(result, 0, 1), sizeof(random_number));
		random_number = ntohl(random_number);
		PQclear(result);

		const yajl_gen gen = get_json_generator(&query_ctx->req->pool);

		if (gen) {
			// The response is small enough, so that it is simpler to copy it
			// instead of doing a delayed deallocation of the JSON generator.
			if (!serialize_item(ntohl(query_ctx->id), random_number, gen) &&
			    !send_json_response(gen, true, query_ctx->req))
				return DONE;

			yajl_gen_free(gen);
		}

		send_error(INTERNAL_SERVER_ERROR, MEM_ALLOC_ERR_MSG, query_ctx->req);
	}
	else {
		send_error(BAD_GATEWAY, PQresultErrorMessage(result), query_ctx->req);
		PQclear(result);
	}

	return DONE;
}

static void on_single_query_timeout(db_query_param_t *param)
{
	single_query_ctx_t * const query_ctx = H2O_STRUCT_FROM_MEMBER(single_query_ctx_t,
	                                                              param,
	                                                              param);

	send_error(GATEWAY_TIMEOUT, DB_TIMEOUT_ERROR, query_ctx->req);
}

static void on_update_error(db_query_param_t *param, const char *error_string)
{
	const update_param_t * const query_param = H2O_STRUCT_FROM_MEMBER(update_param_t,
	                                                                  param,
	                                                                  param);
	update_ctx_t * const update_ctx = query_param->ctx;

	if (update_ctx->gen) {
		yajl_gen_free(update_ctx->gen);
		update_ctx->gen = NULL;
		send_error(BAD_GATEWAY, error_string, update_ctx->req);
	}

	update_ctx->num_query_in_progress--;
	h2o_mem_release_shared(update_ctx);
}

static result_return_t on_update_result(db_query_param_t *param, PGresult *result)
{
	update_param_t * const update_param = H2O_STRUCT_FROM_MEMBER(update_param_t,
	                                                             param,
	                                                             param);
	update_ctx_t * const update_ctx = update_param->ctx;
	result_return_t ret = DONE;

	if (update_ctx->gen) {
		if (update_param->update) {
			if (PQresultStatus(result) == PGRES_COMMAND_OK) {
				thread_context_t * const ctx =
					H2O_STRUCT_FROM_MEMBER(thread_context_t,
					                       event_loop.h2o_ctx,
					                       update_ctx->req->conn->ctx);
				const size_t num_query_remaining =
					update_ctx->num_query - update_ctx->num_result;

				update_ctx->num_query_in_progress--;

				if (update_ctx->num_query_in_progress < num_query_remaining) {
					const size_t idx = update_ctx->num_result +
					                   update_ctx->num_query_in_progress;

					update_param->random_number =
						get_random_number(MAX_ID, &ctx->random_seed) + 1;
					update_param->update = false;
					snprintf(update_param->command,
					         MAX_UPDATE_QUERY_LEN,
					         UPDATE_QUERY,
					         update_ctx->res[idx].id,
					         update_ctx->res[idx].id,
					         update_param->random_number);

					if (!execute_query(ctx, &update_param->param)) {
						update_ctx->num_query_in_progress++;
						PQclear(result);
						return DONE;
					}

					yajl_gen_free(update_ctx->gen);
					update_ctx->gen = NULL;
					send_service_unavailable_error(DB_REQ_ERROR,
					                               update_ctx->req);
				}
				else if (update_ctx->num_result == update_ctx->num_query)
					serialize_items(update_ctx->res,
					                update_ctx->num_result,
					                update_ctx->gen,
					                update_ctx->req);

				h2o_mem_release_shared(update_ctx);
			}
			else
				on_update_error(param, PQresultErrorMessage(result));
		}
		else if (PQresultStatus(result) == PGRES_TUPLES_OK) {
			process_result(result, update_ctx->res + update_ctx->num_result);
			update_ctx->res[update_ctx->num_result].random_number =
				update_param->random_number;
			update_ctx->num_result++;
			update_param->update = true;
			ret = SUCCESS;
		}
		else
			on_update_error(param, PQresultErrorMessage(result));
	}
	else {
		update_ctx->num_query_in_progress--;
		h2o_mem_release_shared(update_ctx);
	}

	PQclear(result);
	return ret;
}

static void on_update_timeout(db_query_param_t *param)
{
	const update_param_t * const query_param = H2O_STRUCT_FROM_MEMBER(update_param_t,
	                                                                  param,
	                                                                  param);
	update_ctx_t * const update_ctx = query_param->ctx;

	if (update_ctx->gen) {
		yajl_gen_free(update_ctx->gen);
		update_ctx->gen = NULL;
		send_error(GATEWAY_TIMEOUT, DB_TIMEOUT_ERROR, update_ctx->req);
	}

	update_ctx->num_query_in_progress--;
	h2o_mem_release_shared(update_ctx);
}

static void process_result(PGresult *result, query_result_t *out)
{
	assert(PQnfields(result) == 2);
	assert(PQntuples(result) == 1);

	const char * const id = PQgetvalue(result, 0, 0);
	const char * const random_number = PQgetvalue(result, 0, 1);

	if (PQfformat(result, 0)) {
		assert(PQgetlength(result, 0, 0) == sizeof(out->id));
		// Use memcpy() in case the result is not aligned; the reason we are
		// copying over the id is because the results may arrive in any order.
		memcpy(&out->id, id, sizeof(out->id));
		out->id = ntohl(out->id);
	}
	else
		out->id = atoi(id);

	if (PQfformat(result, 1)) {
		assert(PQgetlength(result, 0, 1) == sizeof(out->random_number));
		// Use memcpy() in case the result is not aligned.
		memcpy(&out->random_number, random_number, sizeof(out->random_number));
		out->random_number = ntohl(out->random_number);
	}
	else
		out->random_number = atoi(random_number);
}

static int serialize_item(uint32_t id, uint32_t random_number, yajl_gen gen)
{
	CHECK_YAJL_STATUS(yajl_gen_map_open, gen);
	CHECK_YAJL_STATUS(yajl_gen_string, gen, YAJL_STRLIT(ID_KEY));
	CHECK_YAJL_STATUS(yajl_gen_integer, gen, id);
	CHECK_YAJL_STATUS(yajl_gen_string, gen, YAJL_STRLIT(RANDOM_NUM_KEY));
	CHECK_YAJL_STATUS(yajl_gen_integer, gen, random_number);
	CHECK_YAJL_STATUS(yajl_gen_map_close, gen);
	return EXIT_SUCCESS;
error_yajl:
	return EXIT_FAILURE;
}

static void serialize_items(const query_result_t *res,
                            size_t num_result,
                            yajl_gen gen,
                            h2o_req_t *req)
{
	CHECK_YAJL_STATUS(yajl_gen_array_open, gen);

	for (size_t i = 0; i < num_result; i++)
		if (serialize_item(res[i].id, res[i].random_number, gen))
			goto error_yajl;

	CHECK_YAJL_STATUS(yajl_gen_array_close, gen);

	if (!send_json_response(gen, false, req))
		return;

error_yajl:
	send_error(INTERNAL_SERVER_ERROR, MEM_ALLOC_ERR_MSG, req);
}

int multiple_queries(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);

	thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
	                                                      event_loop.h2o_ctx,
	                                                      req->conn->ctx);

	const size_t num_query = get_query_number(req);
	size_t base_size = offsetof(multiple_query_ctx_t, res) + num_query * sizeof(query_result_t);

	base_size = ((base_size + _Alignof(query_param_t) - 1) / _Alignof(query_param_t));
	base_size = base_size * _Alignof(query_param_t);

	const size_t num_query_in_progress = MIN(num_query, ctx->config->max_db_conn_num);
	const size_t sz = base_size + num_query_in_progress * sizeof(query_param_t);

	multiple_query_ctx_t * const query_ctx = h2o_mem_alloc_shared(&req->pool,
	                                                              sz,
	                                                              cleanup_multiple_query);

	if (query_ctx) {
		memset(query_ctx, 0, sz);
		query_ctx->num_query = num_query;
		query_ctx->num_query_in_progress = num_query_in_progress;
		query_ctx->req = req;
		query_ctx->query_param = (query_param_t *) ((char *) query_ctx + base_size);
		initialize_ids(num_query, query_ctx->res, &ctx->random_seed);

		for (size_t i = 0; i < query_ctx->num_query_in_progress; i++) {
			query_ctx->query_param[i].ctx = query_ctx;
			// We need a copy of id because the original may be overwritten
			// by a completed query.
			query_ctx->query_param[i].id = htonl(query_ctx->res[i].id);
			query_ctx->query_param[i].id_format = 1;
			query_ctx->query_param[i].id_len = sizeof(query_ctx->query_param[i].id);
			query_ctx->query_param[i].id_pointer =
				(const char *) &query_ctx->query_param[i].id;
			query_ctx->query_param[i].param.command = WORLD_TABLE_NAME;
			query_ctx->query_param[i].param.nParams = 1;
			query_ctx->query_param[i].param.on_error = on_multiple_query_error;
			query_ctx->query_param[i].param.on_result = on_multiple_query_result;
			query_ctx->query_param[i].param.on_timeout = on_multiple_query_timeout;
			query_ctx->query_param[i].param.paramFormats =
				&query_ctx->query_param[i].id_format;
			query_ctx->query_param[i].param.paramLengths =
				&query_ctx->query_param[i].id_len;
			query_ctx->query_param[i].param.paramValues =
				&query_ctx->query_param[i].id_pointer;
			query_ctx->query_param[i].param.flags = IS_PREPARED;
			query_ctx->query_param[i].param.resultFormat = 1;

			if (execute_query(ctx, &query_ctx->query_param[i].param)) {
				query_ctx->num_query_in_progress = i;
				send_service_unavailable_error(DB_REQ_ERROR, req);
				return 0;
			}

			h2o_mem_addref_shared(query_ctx);
		}

		// Create a JSON generator while the queries are processed.
		query_ctx->gen = get_json_generator(&req->pool);

		if (!query_ctx->gen)
			send_error(INTERNAL_SERVER_ERROR, MEM_ALLOC_ERR_MSG, req);
	}
	else
		send_error(INTERNAL_SERVER_ERROR, MEM_ALLOC_ERR_MSG, req);

	return 0;
}

int single_query(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);

	thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
	                                                      event_loop.h2o_ctx,
	                                                      req->conn->ctx);
	single_query_ctx_t * const query_ctx = h2o_mem_alloc_pool(&req->pool, sizeof(*query_ctx));

	if (query_ctx) {
		memset(query_ctx, 0, sizeof(*query_ctx));
		query_ctx->id = htonl(get_random_number(MAX_ID, &ctx->random_seed) + 1);
		query_ctx->id_format = 1;
		query_ctx->id_len = sizeof(query_ctx->id);
		query_ctx->id_pointer = (const char *) &query_ctx->id;
		query_ctx->param.command = WORLD_TABLE_NAME;
		query_ctx->param.nParams = 1;
		query_ctx->param.on_error = on_single_query_error;
		query_ctx->param.on_result = on_single_query_result;
		query_ctx->param.on_timeout = on_single_query_timeout;
		query_ctx->param.paramFormats = &query_ctx->id_format;
		query_ctx->param.paramLengths = &query_ctx->id_len;
		query_ctx->param.paramValues = &query_ctx->id_pointer;
		query_ctx->param.flags = IS_PREPARED;
		query_ctx->param.resultFormat = 1;
		query_ctx->req = req;

		if (execute_query(ctx, &query_ctx->param))
			send_service_unavailable_error(DB_REQ_ERROR, req);
	}
	else
		send_error(INTERNAL_SERVER_ERROR, MEM_ALLOC_ERR_MSG, req);

	return 0;
}

int updates(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);

	thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
	                                                      event_loop.h2o_ctx,
	                                                      req->conn->ctx);

	const size_t num_query = get_query_number(req);
	size_t base_size = offsetof(update_ctx_t, res) + num_query * sizeof(query_result_t);

	base_size = ((base_size + _Alignof(update_param_t) - 1) / _Alignof(update_param_t));
	base_size = base_size * _Alignof(update_param_t);

	const size_t num_query_in_progress = MIN(num_query, ctx->config->max_db_conn_num);
	const size_t struct_size = base_size + num_query_in_progress * sizeof(update_param_t);
	const size_t sz = struct_size + num_query_in_progress * MAX_UPDATE_QUERY_LEN;
	update_ctx_t * const update_ctx = h2o_mem_alloc_shared(&req->pool, sz, cleanup_update);

	if (update_ctx) {
		memset(update_ctx, 0, struct_size);
		update_ctx->num_query = num_query;
		update_ctx->num_query_in_progress = num_query_in_progress;
		update_ctx->req = req;
		update_ctx->update_param = (update_param_t *) ((char *) update_ctx + base_size);
		initialize_ids(num_query, update_ctx->res, &ctx->random_seed);

		char *command = (char *) update_ctx + struct_size;

		for (size_t i = 0; i < update_ctx->num_query_in_progress; i++) {
			update_ctx->update_param[i].command = command;
			command += MAX_UPDATE_QUERY_LEN;
			update_ctx->update_param[i].ctx = update_ctx;
			update_ctx->update_param[i].param.command =
				update_ctx->update_param[i].command;
			update_ctx->update_param[i].param.on_error = on_update_error;
			update_ctx->update_param[i].param.on_result = on_update_result;
			update_ctx->update_param[i].param.on_timeout = on_update_timeout;
			update_ctx->update_param[i].param.resultFormat = 1;
			update_ctx->update_param[i].random_number =
				get_random_number(MAX_ID, &ctx->random_seed) + 1;
			snprintf(update_ctx->update_param[i].command,
			         MAX_UPDATE_QUERY_LEN,
			         UPDATE_QUERY,
			         update_ctx->res[i].id,
			         update_ctx->res[i].id,
			         update_ctx->update_param[i].random_number);

			if (execute_query(ctx, &update_ctx->update_param[i].param)) {
				update_ctx->num_query_in_progress = i;
				send_service_unavailable_error(DB_REQ_ERROR, req);
				return 0;
			}

			h2o_mem_addref_shared(update_ctx);
		}

		// Create a JSON generator while the queries are processed.
		update_ctx->gen = get_json_generator(&req->pool);

		if (!update_ctx->gen)
			send_error(INTERNAL_SERVER_ERROR, MEM_ALLOC_ERR_MSG, req);
	}
	else
		send_error(INTERNAL_SERVER_ERROR, MEM_ALLOC_ERR_MSG, req);

	return 0;
}
