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
#include <stddef.h>
#include <stdint.h>
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

typedef enum {
	NO_UPDATE = 0,
	CREATE,
	COPY_1,
	COPY_2,
	UPDATE
} update_state_t;

typedef struct {
	uint32_t id;
	uint32_t random_number;
} query_result_t;

typedef struct {
	db_query_param_t param;
	yajl_gen gen;
	const char *id_pointer;
	h2o_req_t *req;
	uint32_t id;
	int id_format;
	int id_len;
} single_query_ctx_t;

typedef struct {
	single_query_ctx_t single;
	size_t num_query;
	size_t num_result;
	update_state_t update_state;
	query_result_t res[];
} multiple_query_ctx_t;

static int do_multiple_queries(update_state_t update_state, h2o_req_t *req);
static int initialize_single_query_context(h2o_req_t *req,
                                           on_result_t on_result,
                                           single_query_ctx_t *query_ctx);
static void on_database_error(db_query_param_t *param, const char *error_string);
static void on_database_timeout(db_query_param_t *param);
static result_return_t on_multiple_query_result(db_query_param_t *param, PGresult *result);
static result_return_t on_single_query_result(db_query_param_t *param, PGresult *result);
static result_return_t on_update_result(db_query_param_t *param, PGresult *result);
static int on_update_write_ready(db_query_param_t *param, PGconn *db_conn);
static int serialize_item(uint32_t id, uint32_t random_number, yajl_gen gen);
static void serialize_items(const query_result_t *res,
                            size_t num_result,
                            yajl_gen gen,
                            h2o_req_t *req);

static int do_multiple_queries(update_state_t update_state, h2o_req_t *req)
{
	thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
	                                                      event_loop.h2o_ctx,
	                                                      req->conn->ctx);
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

	const size_t sz = offsetof(multiple_query_ctx_t, res) + num_query * sizeof(query_result_t);
	multiple_query_ctx_t * const query_ctx = h2o_mem_alloc_pool(&req->pool, sz);

	if (!query_ctx || initialize_single_query_context(req,
	                                                  on_multiple_query_result,
	                                                  &query_ctx->single))
		send_error(INTERNAL_SERVER_ERROR, MEM_ALLOC_ERR_MSG, req);
	else {
		// MAX_ID is a relatively small number, so allocate on the stack.
		DEFINE_BITSET(bitset, MAX_ID);

		memset(&query_ctx->single + 1,
		       0,
		       offsetof(multiple_query_ctx_t, res) - sizeof(query_ctx->single));
		query_ctx->num_query = num_query;
		query_ctx->update_state = update_state;

		size_t max_rand = MAX_ID - query_ctx->num_query + 1;

		for (size_t i = 0; i < query_ctx->num_query; i++) {
			query_ctx->res[i].id = get_random_number(max_rand, &ctx->random_seed);

			if (BITSET_ISSET(query_ctx->res[i].id, bitset))
				query_ctx->res[i].id = max_rand - 1;

			BITSET_SET(query_ctx->res[i].id++, bitset);
			max_rand++;
		}

		query_ctx->single.id = htonl(query_ctx->res->id);

		if (execute_query(ctx, &query_ctx->single.param)) {
			yajl_gen_free(query_ctx->single.gen);
			send_service_unavailable_error(DB_REQ_ERROR, req);
		}
	}

	return 0;
}

static int initialize_single_query_context(h2o_req_t *req,
                                           on_result_t on_result,
                                           single_query_ctx_t *query_ctx)
{
	int ret = EXIT_FAILURE;

	memset(query_ctx, 0, sizeof(*query_ctx));
	query_ctx->gen = get_json_generator(&req->pool);

	if (query_ctx->gen) {
		query_ctx->id_format = 1;
		query_ctx->id_len = sizeof(query_ctx->id);
		query_ctx->id_pointer = (const char *) &query_ctx->id;
		query_ctx->param.command = WORLD_TABLE_NAME;
		query_ctx->param.nParams = 1;
		query_ctx->param.on_error = on_database_error;
		query_ctx->param.on_result = on_result;
		query_ctx->param.on_timeout = on_database_timeout;
		query_ctx->param.paramFormats = &query_ctx->id_format;
		query_ctx->param.paramLengths = &query_ctx->id_len;
		query_ctx->param.paramValues = &query_ctx->id_pointer;
		query_ctx->param.flags = IS_PREPARED;
		query_ctx->param.resultFormat = 1;
		query_ctx->req = req;
		ret = EXIT_SUCCESS;
	}

	return ret;
}

static void on_database_error(db_query_param_t *param, const char *error_string)
{
	single_query_ctx_t * const query_ctx = H2O_STRUCT_FROM_MEMBER(single_query_ctx_t,
	                                                              param,
	                                                              param);

	yajl_gen_free(query_ctx->gen);
	send_error(BAD_GATEWAY, error_string, query_ctx->req);
}

static void on_database_timeout(db_query_param_t *param)
{
	single_query_ctx_t * const query_ctx = H2O_STRUCT_FROM_MEMBER(single_query_ctx_t,
	                                                              param,
	                                                              param);

	yajl_gen_free(query_ctx->gen);
	send_error(GATEWAY_TIMEOUT, DB_TIMEOUT_ERROR, query_ctx->req);
}

static result_return_t on_multiple_query_result(db_query_param_t *param, PGresult *result)
{
	multiple_query_ctx_t * const query_ctx = H2O_STRUCT_FROM_MEMBER(multiple_query_ctx_t,
	                                                                single.param,
	                                                                param);

	if (PQresultStatus(result) == PGRES_TUPLES_OK) {
		thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
		                                                      event_loop.h2o_ctx,
		                                                      query_ctx->single.req->conn->ctx);
		uint32_t * const random_number = &query_ctx->res[query_ctx->num_result++].random_number;

		assert(PQnfields(result) == 2);
		assert(PQntuples(result) == 1);
		assert(PQgetlength(result, 0, 1) == sizeof(*random_number));
		// Use memcpy() in case the result is not aligned.
		memcpy(random_number, PQgetvalue(result, 0, 1), sizeof(*random_number));
		*random_number = ntohl(*random_number);
		PQclear(result);

		if (query_ctx->num_result < query_ctx->num_query) {
			query_ctx->single.id = htonl(query_ctx->res[query_ctx->num_result].id);

			if (!execute_query(ctx, &query_ctx->single.param))
				return DONE;

			send_service_unavailable_error(DB_REQ_ERROR, query_ctx->single.req);
		}
		else if (query_ctx->update_state == NO_UPDATE) {
			serialize_items(query_ctx->res,
			                query_ctx->num_result,
			                query_ctx->single.gen,
			                query_ctx->single.req);
			return DONE;
		}
		else {
			query_ctx->single.param.command = UPDATE_QUERY;
			query_ctx->single.param.nParams = 0;
			query_ctx->single.param.on_result = on_update_result;
			query_ctx->single.param.on_write_ready = on_update_write_ready;
			query_ctx->single.param.paramFormats = NULL;
			query_ctx->single.param.paramLengths = NULL;
			query_ctx->single.param.paramValues = NULL;
			query_ctx->single.param.flags = 0;

			if (!execute_query(ctx, &query_ctx->single.param))
				return DONE;

			send_service_unavailable_error(DB_REQ_ERROR, query_ctx->single.req);
		}
	}
	else {
		send_error(BAD_GATEWAY, PQresultErrorMessage(result), query_ctx->single.req);
		PQclear(result);
	}

	yajl_gen_free(query_ctx->single.gen);
	return DONE;
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

		if (!serialize_item(ntohl(query_ctx->id), random_number, query_ctx->gen) &&
		    !send_json_response(query_ctx->gen, NULL, query_ctx->req))
			return DONE;

		send_error(INTERNAL_SERVER_ERROR, MEM_ALLOC_ERR_MSG, query_ctx->req);
	}
	else {
		send_error(BAD_GATEWAY, PQresultErrorMessage(result), query_ctx->req);
		PQclear(result);
	}

	yajl_gen_free(query_ctx->gen);
	return DONE;
}

static result_return_t on_update_result(db_query_param_t *param, PGresult *result)
{
	multiple_query_ctx_t * const query_ctx = H2O_STRUCT_FROM_MEMBER(multiple_query_ctx_t,
	                                                                single.param,
	                                                                param);
	result_return_t ret = SUCCESS;
	const ExecStatusType status = PQresultStatus(result);

	switch (query_ctx->update_state) {
		case CREATE:
		case COPY_2:
			if (status != PGRES_COMMAND_OK)
				goto error;

			query_ctx->update_state++;
			break;
		case COPY_1:
			if (status != PGRES_COPY_IN)
				goto error;

			ret = WANT_WRITE;
			break;
		case UPDATE:
			if (status != PGRES_COMMAND_OK)
				goto error;

			serialize_items(query_ctx->res,
			                query_ctx->num_result,
			                query_ctx->single.gen,
			                query_ctx->single.req);
			ret = DONE;
			break;
		default:
			goto error;
	}

	PQclear(result);
	return ret;
error:
	yajl_gen_free(query_ctx->single.gen);
	send_error(BAD_GATEWAY, PQresultErrorMessage(result), query_ctx->single.req);
	PQclear(result);
	return DONE;
}

static int on_update_write_ready(db_query_param_t *param, PGconn *db_conn)
{
	multiple_query_ctx_t * const query_ctx = H2O_STRUCT_FROM_MEMBER(multiple_query_ctx_t,
	                                                                single.param,
	                                                                param);
	thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
	                                                      event_loop.h2o_ctx,
	                                                      query_ctx->single.req->conn->ctx);

	if (query_ctx->num_result == query_ctx->num_query && query_ctx->update_state != COPY_2) {
		const int rc = PQputCopyData(db_conn, H2O_STRLIT(COPY_HEADER));

		if (!rc)
			return 1;
		else if (rc < 0)
			return rc;

		query_ctx->num_result = 0;
		query_ctx->update_state = COPY_2;
	}

	if (query_ctx->num_result < query_ctx->num_query) {
		assert(query_ctx->num_query - query_ctx->num_result <= MAX_QUERIES);

		// There are at most MAX_QUERIES elements, so allocate on the stack.
		struct __attribute__ ((__packed__)) {
			uint16_t field_count;
			uint32_t id_size;
			uint32_t id;
			uint32_t random_number_size;
			uint32_t random_number;
		} data[query_ctx->num_query - query_ctx->num_result];

		memset(&data, 0, sizeof(data));

		for (size_t i = 0; i < query_ctx->num_query; i++) {
			query_ctx->res[i].random_number = get_random_number(MAX_ID, &ctx->random_seed) + 1;
			data[i].field_count = htons(2);
			data[i].id_size = htonl(sizeof(data->id));
			data[i].id = htonl(query_ctx->res[i].id);
			data[i].random_number_size = htonl(sizeof(data->random_number));
			data[i].random_number = htonl(query_ctx->res[i].random_number);
		}

		const int rc = PQputCopyData(db_conn, (const char *) &data, sizeof(data));

		if (!rc)
			return 1;
		else if (rc < 0)
			return rc;

		query_ctx->num_result = query_ctx->num_query;
	}

	if (query_ctx->num_result == query_ctx->num_query) {
		const int rc = PQputCopyEnd(db_conn, NULL);

		if (!rc)
			return 1;
		else if (rc < 0)
			return rc;
	}

	return PQflush(db_conn);
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

	if (send_json_response(gen, NULL, req)) {
error_yajl:
		yajl_gen_free(gen);
		send_error(INTERNAL_SERVER_ERROR, MEM_ALLOC_ERR_MSG, req);
	}
}

int multiple_queries(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);

	return do_multiple_queries(NO_UPDATE, req);
}

int single_query(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);

	thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
	                                                      event_loop.h2o_ctx,
	                                                      req->conn->ctx);
	single_query_ctx_t * const query_ctx = h2o_mem_alloc_pool(&req->pool, sizeof(*query_ctx));

	if (!query_ctx || initialize_single_query_context(req,
	                                                  on_single_query_result,
	                                                  query_ctx))
		send_error(INTERNAL_SERVER_ERROR, MEM_ALLOC_ERR_MSG, req);
	else {
		query_ctx->id = htonl(get_random_number(MAX_ID, &ctx->random_seed) + 1);

		if (execute_query(ctx, &query_ctx->param)) {
			yajl_gen_free(query_ctx->gen);
			send_service_unavailable_error(DB_REQ_ERROR, req);
		}
	}

	return 0;
}

int updates(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);

	return do_multiple_queries(CREATE, req);
}
