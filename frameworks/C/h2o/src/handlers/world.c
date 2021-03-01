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
#include <ctype.h>
#include <h2o.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include <h2o/cache.h>
#include <postgresql/libpq-fe.h>
#include <yajl/yajl_gen.h>

#include "bitset.h"
#include "cache.h"
#include "database.h"
#include "error.h"
#include "global_data.h"
#include "request_handler.h"
#include "thread.h"
#include "utility.h"
#include "world.h"

#define CACHE_CAPACITY 131072
#define CACHE_DURATION 3600000
#define DO_CLEANUP 4
#define DO_UPDATE 1
#define ID_KEY "id"
#define IS_COMPLETED 8
#define MAX_ID 10000
#define MAX_QUERIES 500
#define WORLD_TABLE_NAME "World"
#define POPULATE_CACHE_QUERY "SELECT * FROM " WORLD_TABLE_NAME ";"
#define QUERIES_PARAMETER "queries="
#define RANDOM_NUM_KEY "randomNumber"

// MAX_UPDATE_QUERY_LEN must be updated whenever UPDATE_QUERY_BEGIN, UPDATE_QUERY_ELEM,
// and UPDATE_QUERY_END are changed.
#define UPDATE_QUERY_BEGIN \
	"UPDATE " WORLD_TABLE_NAME " SET randomNumber = v.randomNumber " \
	"FROM (VALUES(%" PRIu32 ", %" PRIu32 ")"

#define UPDATE_QUERY_ELEM ", (%" PRIu32 ", %" PRIu32 ")"
#define UPDATE_QUERY_END ") AS v (id, randomNumber) WHERE " WORLD_TABLE_NAME ".id = v.id;"

#define MAX_UPDATE_QUERY_LEN(n) \
	(sizeof(UPDATE_QUERY_BEGIN) + sizeof(UPDATE_QUERY_END) - sizeof(UPDATE_QUERY_ELEM) + \
	 (n) * (sizeof(UPDATE_QUERY_ELEM) - 1 + \
	        2 * (sizeof(MKSTR(MAX_ID)) - 1) - 2 * (sizeof(PRIu32) - 1) - 2))

#define USE_CACHE 2
#define WORLD_QUERY "SELECT * FROM " WORLD_TABLE_NAME " WHERE id = $1::integer;"

typedef struct multiple_query_ctx_t multiple_query_ctx_t;
typedef struct update_ctx_t update_ctx_t;

typedef struct {
	thread_context_t *ctx;
	db_query_param_t param;
} populate_cache_ctx_t;

typedef struct {
	multiple_query_ctx_t *ctx;
	const char *id_pointer;
	uint32_t id;
	int id_format;
	int id_len;
	db_query_param_t param;
} query_param_t;

typedef struct {
	uint32_t id;
	uint32_t random_number;
} query_result_t;

typedef struct {
	const char *id_pointer;
	h2o_req_t *req;
	uint32_t id;
	int id_format;
	int id_len;
	bool cleanup;
	db_query_param_t param;
} single_query_ctx_t;

struct multiple_query_ctx_t {
	thread_context_t *ctx;
	json_generator_t *gen;
	h2o_req_t *req;
	query_param_t *query_param;
	size_t num_query;
	size_t num_query_in_progress;
	size_t num_result;
	uint_fast32_t flags;
	query_result_t res[];
};

static int cached_queries(struct st_h2o_handler_t *self, h2o_req_t *req);
static void cleanup_multiple_query(multiple_query_ctx_t *query_ctx);
static void cleanup_multiple_query_request(void *data);
static void cleanup_single_query(single_query_ctx_t *query_ctx);
static void cleanup_single_query_request(void *data);
static int compare_items(const void *x, const void *y);
static void complete_multiple_query(multiple_query_ctx_t *query_ctx);
static int do_multiple_queries(bool do_update, bool use_cache, h2o_req_t *req);
static void do_updates(multiple_query_ctx_t *query_ctx);
static void fetch_from_cache(uint64_t now, cache_t *cache, multiple_query_ctx_t *query_ctx);
static void free_cache_entry(h2o_iovec_t value);
static size_t get_query_number(h2o_req_t *req);
static void initialize_ids(size_t num_query, query_result_t *res, unsigned int *seed);
static int multiple_queries(struct st_h2o_handler_t *self, h2o_req_t *req);
static void on_multiple_query_error(db_query_param_t *param, const char *error_string);
static result_return_t on_multiple_query_result(db_query_param_t *param, PGresult *result);
static void on_multiple_query_timeout(db_query_param_t *param);
static void on_populate_cache_error(db_query_param_t *param, const char *error_string);
static result_return_t on_populate_cache_result(db_query_param_t *param, PGresult *result);
static void on_populate_cache_timeout(db_query_param_t *param);
static void on_single_query_error(db_query_param_t *param, const char *error_string);
static result_return_t on_single_query_result(db_query_param_t *param, PGresult *result);
static void on_single_query_timeout(db_query_param_t *param);
static result_return_t on_update_result(db_query_param_t *param, PGresult *result);
static void populate_cache(thread_context_t *ctx, void *arg);
static void process_result(PGresult *result, size_t idx, query_result_t *out);
static int serialize_item(uint32_t id, uint32_t random_number, yajl_gen gen);
static void serialize_items(const query_result_t *res,
                            size_t num_result,
                            json_generator_t **gen,
                            h2o_req_t *req);
static int single_query(struct st_h2o_handler_t *self, h2o_req_t *req);
static int updates(struct st_h2o_handler_t *self, h2o_req_t *req);


static int cached_queries(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);
	return do_multiple_queries(false, true, req);
}

static void cleanup_multiple_query(multiple_query_ctx_t *query_ctx)
{
	if (query_ctx->gen)
		free_json_generator(query_ctx->gen,
		                    &query_ctx->ctx->json_generator,
		                    &query_ctx->ctx->json_generator_num,
		                    query_ctx->ctx->config->max_json_generator);

	free(query_ctx);
}

static void cleanup_multiple_query_request(void *data)
{
	multiple_query_ctx_t * const query_ctx = *(multiple_query_ctx_t **) data;

	query_ctx->flags |= IS_COMPLETED;

	if (query_ctx->flags & DO_CLEANUP) {
		if (!query_ctx->num_query_in_progress)
			cleanup_multiple_query(query_ctx);
	}
	else
		query_ctx->flags |= DO_CLEANUP;
}

static void cleanup_single_query(single_query_ctx_t *query_ctx)
{
	free(query_ctx);
}

static void cleanup_single_query_request(void *data)
{
	single_query_ctx_t * const query_ctx = *(single_query_ctx_t **) data;

	if (query_ctx->cleanup)
		cleanup_single_query(query_ctx);
	else
		query_ctx->cleanup = true;
}

static int compare_items(const void *x, const void *y)
{
	const query_result_t * const r1 = x;
	const query_result_t * const r2 = y;

	return r1->id < r2->id ? -1 : r1->id > r2->id;
}

static void complete_multiple_query(multiple_query_ctx_t *query_ctx)
{
	assert(query_ctx->num_result == query_ctx->num_query);

	if (query_ctx->flags & DO_UPDATE)
		do_updates(query_ctx);
	else {
		query_ctx->flags |= DO_CLEANUP;
		query_ctx->gen = get_json_generator(&query_ctx->ctx->json_generator,
		                                    &query_ctx->ctx->json_generator_num);

		if (query_ctx->gen)
			serialize_items(query_ctx->res,
			                query_ctx->num_result,
			                &query_ctx->gen,
			                query_ctx->req);
		else
			send_error(INTERNAL_SERVER_ERROR, REQ_ERROR, query_ctx->req);
	}
}

static int do_multiple_queries(bool do_update, bool use_cache, h2o_req_t *req)
{
	thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
	                                                      event_loop.h2o_ctx,
	                                                      req->conn->ctx);

	const size_t num_query = get_query_number(req);

	// MAX_QUERIES is a relatively small number, so assume no overflow in the following
	// arithmetic operations.
	assert(num_query <= MAX_QUERIES);

	size_t base_size = offsetof(multiple_query_ctx_t, res) + num_query * sizeof(query_result_t);

	base_size = ((base_size + _Alignof(query_param_t) - 1) / _Alignof(query_param_t));
	base_size = base_size * _Alignof(query_param_t);

	const size_t num_query_in_progress = MIN(num_query, ctx->config->max_db_conn_num);
	size_t sz = base_size + num_query_in_progress * sizeof(query_param_t);

	if (do_update) {
		const size_t reuse_size = (num_query_in_progress - 1) * sizeof(query_param_t);
		const size_t update_query_len = MAX_UPDATE_QUERY_LEN(num_query);

		if (update_query_len > reuse_size)
			sz += update_query_len - reuse_size;
	}

	multiple_query_ctx_t * const query_ctx = h2o_mem_alloc(sz);
	multiple_query_ctx_t ** const p = h2o_mem_alloc_shared(&req->pool,
	                                                       sizeof(*p),
	                                                       cleanup_multiple_query_request);

	*p = query_ctx;
	memset(query_ctx, 0, sz);
	query_ctx->ctx = ctx;
	query_ctx->num_query = num_query;
	query_ctx->req = req;
	query_ctx->query_param = (query_param_t *) ((char *) query_ctx + base_size);
	initialize_ids(num_query, query_ctx->res, &ctx->random_seed);

	if (do_update)
		query_ctx->flags |= DO_UPDATE;

	if (use_cache) {
		query_ctx->flags |= USE_CACHE;
		fetch_from_cache(h2o_now(ctx->event_loop.h2o_ctx.loop),
		                 &ctx->global_data->request_handler_data.world_cache,
		                 query_ctx);

		if (query_ctx->num_result == query_ctx->num_query) {
			complete_multiple_query(query_ctx);
			return 0;
		}
	}

	query_ctx->num_query_in_progress = MIN(num_query_in_progress,
	                                       query_ctx->num_query - query_ctx->num_result);

	// Keep this loop separate, so that it could be vectorized.
	for (size_t i = 0; i < query_ctx->num_query_in_progress; i++) {
		query_ctx->query_param[i].ctx = query_ctx;
		// We need a copy of id because the original may be overwritten
		// by a completed query.
		query_ctx->query_param[i].id = htonl(query_ctx->res[query_ctx->num_result + i].id);
		query_ctx->query_param[i].id_format = 1;
		query_ctx->query_param[i].id_len = sizeof(query_ctx->query_param[i].id);
		query_ctx->query_param[i].id_pointer = (const char *) &query_ctx->query_param[i].id;
		query_ctx->query_param[i].param.command = WORLD_TABLE_NAME;
		query_ctx->query_param[i].param.nParams = 1;
		query_ctx->query_param[i].param.on_error = on_multiple_query_error;
		query_ctx->query_param[i].param.on_result = on_multiple_query_result;
		query_ctx->query_param[i].param.on_timeout = on_multiple_query_timeout;
		query_ctx->query_param[i].param.paramFormats = &query_ctx->query_param[i].id_format;
		query_ctx->query_param[i].param.paramLengths = &query_ctx->query_param[i].id_len;
		query_ctx->query_param[i].param.paramValues = &query_ctx->query_param[i].id_pointer;
		query_ctx->query_param[i].param.flags = IS_PREPARED;
		query_ctx->query_param[i].param.resultFormat = 1;
	}

	for (size_t i = 0; i < query_ctx->num_query_in_progress; i++)
		if (execute_query(ctx, &query_ctx->query_param[i].param)) {
			query_ctx->num_query_in_progress = i;
			query_ctx->flags |= DO_CLEANUP;
			send_service_unavailable_error(DB_REQ_ERROR, req);
			break;
		}

	return 0;
}

static void do_updates(multiple_query_ctx_t *query_ctx)
{
	char *iter = (char *) (query_ctx->query_param + 1);
	size_t sz = MAX_UPDATE_QUERY_LEN(query_ctx->num_result);

	// Sort the results to avoid database deadlock.
	qsort(query_ctx->res, query_ctx->num_result, sizeof(*query_ctx->res), compare_items);
	query_ctx->query_param->param.command = iter;
	query_ctx->query_param->param.nParams = 0;
	query_ctx->query_param->param.on_result = on_update_result;
	query_ctx->query_param->param.paramFormats = NULL;
	query_ctx->query_param->param.paramLengths = NULL;
	query_ctx->query_param->param.paramValues = NULL;
	query_ctx->query_param->param.flags = 0;
	query_ctx->res->random_number = 1 + get_random_number(MAX_ID, &query_ctx->ctx->random_seed);

	int c = snprintf(iter,
	                 sz,
	                 UPDATE_QUERY_BEGIN,
	                 query_ctx->res->id,
	                 query_ctx->res->random_number);

	if ((size_t) c >= sz)
		goto error;

	iter += c;
	sz -= c;

	for (size_t i = 1; i < query_ctx->num_result; i++) {
		query_ctx->res[i].random_number = 1 + get_random_number(MAX_ID,
		                                                        &query_ctx->ctx->random_seed);
		c = snprintf(iter,
		             sz,
		             UPDATE_QUERY_ELEM,
		             query_ctx->res[i].id,
		             query_ctx->res[i].random_number);

		if ((size_t) c >= sz)
			goto error;

		iter += c;
		sz -= c;
	}

	c = snprintf(iter, sz, UPDATE_QUERY_END);

	if ((size_t) c >= sz)
		goto error;

	if (execute_query(query_ctx->ctx, &query_ctx->query_param->param)) {
		query_ctx->flags |= DO_CLEANUP;
		send_service_unavailable_error(DB_REQ_ERROR, query_ctx->req);
	}
	else
		query_ctx->num_query_in_progress++;

	return;
error:
	query_ctx->flags |= DO_CLEANUP;
	LIBRARY_ERROR("snprintf", "Truncated output.");
	send_error(INTERNAL_SERVER_ERROR, REQ_ERROR, query_ctx->req);
}

static void fetch_from_cache(uint64_t now, cache_t *cache, multiple_query_ctx_t *query_ctx)
{
	h2o_iovec_t key = {.len = sizeof(query_ctx->res->id)};

	for (size_t i = 0; i < query_ctx->num_query; i++) {
		key.base = (char *) &query_ctx->res[i].id;

		const h2o_cache_hashcode_t keyhash = h2o_cache_calchash(key.base, key.len);
		h2o_cache_ref_t * const r = cache_fetch(cache, now, key, keyhash);

		if (r) {
			query_ctx->res[i].id = query_ctx->res[query_ctx->num_result].id;
			memcpy(query_ctx->res + query_ctx->num_result++,
			       r->value.base,
			       sizeof(*query_ctx->res));
			cache_release(cache, r, keyhash);
		}
	}
}

static void free_cache_entry(h2o_iovec_t value)
{
	free(value.base);
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

static int multiple_queries(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);
	return do_multiple_queries(false, false, req);
}

static void on_multiple_query_error(db_query_param_t *param, const char *error_string)
{
	const query_param_t * const query_param = H2O_STRUCT_FROM_MEMBER(query_param_t, param, param);
	multiple_query_ctx_t * const query_ctx = query_param->ctx;

	query_ctx->num_query_in_progress--;

	if (query_ctx->flags & DO_CLEANUP) {
		if (!query_ctx->num_query_in_progress && query_ctx->flags & IS_COMPLETED)
			cleanup_multiple_query(query_ctx);
	}
	else {
		query_ctx->flags |= DO_CLEANUP;
		send_error(BAD_GATEWAY, error_string, query_ctx->req);
	}
}

static result_return_t on_multiple_query_result(db_query_param_t *param, PGresult *result)
{
	query_param_t * const query_param = H2O_STRUCT_FROM_MEMBER(query_param_t, param, param);
	multiple_query_ctx_t * const query_ctx = query_param->ctx;

	query_ctx->num_query_in_progress--;

	if (query_ctx->flags & DO_CLEANUP) {
		if (!query_ctx->num_query_in_progress && query_ctx->flags & IS_COMPLETED)
			cleanup_multiple_query(query_ctx);
	}
	else if (PQresultStatus(result) == PGRES_TUPLES_OK) {
		assert(PQntuples(result) == 1);
		process_result(result, 0, query_ctx->res + query_ctx->num_result);

		if (query_ctx->flags & USE_CACHE) {
			query_result_t * const r = h2o_mem_alloc(sizeof(*r));
			const h2o_iovec_t key = {.base = (char *) &r->id, .len = sizeof(r->id)};
			const h2o_iovec_t value = {.base = (char *) r, .len = sizeof(*r)};

			*r = query_ctx->res[query_ctx->num_result];
			cache_set(h2o_now(query_ctx->ctx->event_loop.h2o_ctx.loop),
			          key,
			          0,
			          value,
			          &query_ctx->ctx->global_data->request_handler_data.world_cache);
		}

		query_ctx->num_result++;

		const size_t num_query_remaining = query_ctx->num_query - query_ctx->num_result;

		if (query_ctx->num_query_in_progress < num_query_remaining) {
			const size_t idx = query_ctx->num_result + query_ctx->num_query_in_progress;

			query_param->id = htonl(query_ctx->res[idx].id);

			if (execute_query(query_ctx->ctx, &query_param->param)) {
				query_ctx->flags |= DO_CLEANUP;
				send_service_unavailable_error(DB_REQ_ERROR, query_ctx->req);
			}
			else
				query_ctx->num_query_in_progress++;
		}
		else if (query_ctx->num_result == query_ctx->num_query)
			complete_multiple_query(query_ctx);
	}
	else {
		query_ctx->flags |= DO_CLEANUP;
		LIBRARY_ERROR("PQresultStatus", PQresultErrorMessage(result));
		send_error(BAD_GATEWAY, DB_ERROR, query_ctx->req);
	}

	PQclear(result);
	return DONE;
}

static void on_multiple_query_timeout(db_query_param_t *param)
{
	const query_param_t * const query_param = H2O_STRUCT_FROM_MEMBER(query_param_t, param, param);
	multiple_query_ctx_t * const query_ctx = query_param->ctx;

	query_ctx->num_query_in_progress--;

	if (query_ctx->flags & DO_CLEANUP) {
		if (!query_ctx->num_query_in_progress && query_ctx->flags & IS_COMPLETED)
			cleanup_multiple_query(query_ctx);
	}
	else {
		query_ctx->flags |= DO_CLEANUP;
		send_error(GATEWAY_TIMEOUT, DB_TIMEOUT_ERROR, query_ctx->req);
	}
}

static void on_populate_cache_error(db_query_param_t *param, const char *error_string)
{
	IGNORE_FUNCTION_PARAMETER(error_string);
	free(H2O_STRUCT_FROM_MEMBER(populate_cache_ctx_t, param, param));
}

static result_return_t on_populate_cache_result(db_query_param_t *param, PGresult *result)
{
	populate_cache_ctx_t * const query_ctx = H2O_STRUCT_FROM_MEMBER(populate_cache_ctx_t,
	                                                                param,
	                                                                param);

	if (PQresultStatus(result) == PGRES_TUPLES_OK) {
		const size_t num_rows = PQntuples(result);

		for (size_t i = 0; i < num_rows; i++) {
			query_result_t * const r = h2o_mem_alloc(sizeof(*r));

			memset(r, 0, sizeof(*r));
			process_result(result, i, r);

			const h2o_iovec_t key = {.base = (char *) &r->id, .len = sizeof(r->id)};
			const h2o_iovec_t value = {.base = (char *) r, .len = sizeof(*r)};

			cache_set(h2o_now(query_ctx->ctx->event_loop.h2o_ctx.loop),
			          key,
			          0,
			          value,
			          &query_ctx->ctx->global_data->request_handler_data.world_cache);
		}
	}
	else
		LIBRARY_ERROR("PQresultStatus", PQresultErrorMessage(result));

	PQclear(result);
	free(query_ctx);
	return DONE;
}

static void on_populate_cache_timeout(db_query_param_t *param)
{
	free(H2O_STRUCT_FROM_MEMBER(populate_cache_ctx_t, param, param));
}

static void on_single_query_error(db_query_param_t *param, const char *error_string)
{
	single_query_ctx_t * const query_ctx = H2O_STRUCT_FROM_MEMBER(single_query_ctx_t, param, param);

	if (query_ctx->cleanup)
		cleanup_single_query(query_ctx);
	else {
		query_ctx->cleanup = true;
		send_error(BAD_GATEWAY, error_string, query_ctx->req);
	}
}

static result_return_t on_single_query_result(db_query_param_t *param, PGresult *result)
{
	single_query_ctx_t * const query_ctx = H2O_STRUCT_FROM_MEMBER(single_query_ctx_t, param, param);
	const bool cleanup = query_ctx->cleanup;

	query_ctx->cleanup = true;

	if (cleanup)
		cleanup_single_query(query_ctx);
	else if (PQresultStatus(result) == PGRES_TUPLES_OK) {
		uint32_t random_number;

		assert(PQnfields(result) == 2);
		assert(PQntuples(result) == 1);
		assert(PQgetlength(result, 0, 1) == sizeof(random_number));

		const void * const r = PQgetvalue(result, 0, 1);

		assert(r);
		// Use memcpy() in case the result is not aligned.
		memcpy(&random_number, r, sizeof(random_number));
		random_number = ntohl(random_number);

		thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
		                                                      event_loop.h2o_ctx,
		                                                      query_ctx->req->conn->ctx);
		json_generator_t * const gen = get_json_generator(&ctx->json_generator,
		                                                  &ctx->json_generator_num);

		if (gen) {
			// The response is small enough, so that it is simpler to copy it
			// instead of doing a delayed deallocation of the JSON generator.
			if (serialize_item(ntohl(query_ctx->id), random_number, gen->gen) ||
			    send_json_response(gen, true, query_ctx->req)) {
				// If there is a problem with the generator, don't reuse it.
				free_json_generator(gen, NULL, NULL, 0);
				send_error(INTERNAL_SERVER_ERROR, REQ_ERROR, query_ctx->req);
			}
		}
		else
			send_error(INTERNAL_SERVER_ERROR, REQ_ERROR, query_ctx->req);
	}
	else {
		LIBRARY_ERROR("PQresultStatus", PQresultErrorMessage(result));
		send_error(BAD_GATEWAY, DB_ERROR, query_ctx->req);
	}

	PQclear(result);
	return DONE;
}

static void on_single_query_timeout(db_query_param_t *param)
{
	single_query_ctx_t * const query_ctx = H2O_STRUCT_FROM_MEMBER(single_query_ctx_t, param, param);

	if (query_ctx->cleanup)
		cleanup_single_query(query_ctx);
	else {
		query_ctx->cleanup = true;
		send_error(GATEWAY_TIMEOUT, DB_TIMEOUT_ERROR, query_ctx->req);
	}
}

static result_return_t on_update_result(db_query_param_t *param, PGresult *result)
{
	query_param_t * const query_param = H2O_STRUCT_FROM_MEMBER(query_param_t, param, param);
	multiple_query_ctx_t * const query_ctx = query_param->ctx;
	const bool cleanup = query_ctx->flags & DO_CLEANUP;

	query_ctx->flags |= DO_CLEANUP;
	query_ctx->num_query_in_progress--;

	if (cleanup)
		cleanup_multiple_query(query_ctx);
	else if (PQresultStatus(result) == PGRES_COMMAND_OK) {
		query_ctx->gen = get_json_generator(&query_ctx->ctx->json_generator,
		                                    &query_ctx->ctx->json_generator_num);

		if (query_ctx->gen)
			serialize_items(query_ctx->res, query_ctx->num_result, &query_ctx->gen, query_ctx->req);
		else
			send_error(INTERNAL_SERVER_ERROR, REQ_ERROR, query_ctx->req);
	}
	else {
		LIBRARY_ERROR("PQresultStatus", PQresultErrorMessage(result));
		send_error(BAD_GATEWAY, DB_ERROR, query_ctx->req);
	}

	PQclear(result);
	return DONE;
}

static void populate_cache(thread_context_t *ctx, void *arg)
{
	IGNORE_FUNCTION_PARAMETER(arg);

	populate_cache_ctx_t * const query_ctx = h2o_mem_alloc(sizeof(*query_ctx));

	memset(query_ctx, 0, sizeof(*query_ctx));
	query_ctx->ctx = ctx;
	query_ctx->param.command = POPULATE_CACHE_QUERY;
	query_ctx->param.on_error = on_populate_cache_error;
	query_ctx->param.on_result = on_populate_cache_result;
	query_ctx->param.on_timeout = on_populate_cache_timeout;

	if (execute_query(ctx, &query_ctx->param))
		free(query_ctx);
}

static void process_result(PGresult *result, size_t idx, query_result_t *out)
{
	assert(PQnfields(result) == 2);
	assert((size_t) PQntuples(result) > idx);

	const char * const id = PQgetvalue(result, idx, 0);
	const char * const random_number = PQgetvalue(result, idx, 1);

	assert(id && PQgetlength(result, idx, 0) && random_number && PQgetlength(result, idx, 1));

	if (PQfformat(result, 0)) {
		assert(PQgetlength(result, idx, 0) == sizeof(out->id));
		// Use memcpy() in case the result is not aligned; the reason we are
		// copying over the id is because the results may arrive in any order.
		memcpy(&out->id, id, sizeof(out->id));
		out->id = ntohl(out->id);
	}
	else {
		assert(isdigit(*id));
		out->id = atoi(id);
	}

	assert(out->id <= MAX_ID);

	if (PQfformat(result, 1)) {
		assert(PQgetlength(result, idx, 1) == sizeof(out->random_number));
		// Use memcpy() in case the result is not aligned.
		memcpy(&out->random_number, random_number, sizeof(out->random_number));
		out->random_number = ntohl(out->random_number);
	}
	else {
		assert(isdigit(*random_number));
		out->random_number = atoi(random_number);
	}
}

static int serialize_item(uint32_t id, uint32_t random_number, yajl_gen gen)
{
	char buf[32];

	CHECK_YAJL_STATUS(yajl_gen_map_open, gen);
	CHECK_YAJL_STATUS(yajl_gen_string, gen, YAJL_STRLIT(ID_KEY));
	CHECK_YAJL_STATUS(gen_integer, id, buf, sizeof(buf), gen);
	CHECK_YAJL_STATUS(yajl_gen_string, gen, YAJL_STRLIT(RANDOM_NUM_KEY));
	CHECK_YAJL_STATUS(gen_integer, random_number, buf, sizeof(buf), gen);
	CHECK_YAJL_STATUS(yajl_gen_map_close, gen);
	return 0;
error_yajl:
	return 1;
}

static void serialize_items(const query_result_t *res,
                            size_t num_result,
                            json_generator_t **gen,
                            h2o_req_t *req)
{
	CHECK_YAJL_STATUS(yajl_gen_array_open, (*gen)->gen);

	for (size_t i = 0; i < num_result; i++)
		if (serialize_item(res[i].id, res[i].random_number, (*gen)->gen))
			goto error_yajl;

	CHECK_YAJL_STATUS(yajl_gen_array_close, (*gen)->gen);

	if (!send_json_response(*gen, false, req))
		return;

error_yajl:
	// If there is a problem with the generator, don't reuse it.
	free_json_generator(*gen, NULL, NULL, 0);
	*gen = NULL;
	send_error(INTERNAL_SERVER_ERROR, REQ_ERROR, req);
}

static int single_query(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);

	thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
	                                                      event_loop.h2o_ctx,
	                                                      req->conn->ctx);
	single_query_ctx_t * const query_ctx = h2o_mem_alloc(sizeof(*query_ctx));
	single_query_ctx_t ** const p = h2o_mem_alloc_shared(&req->pool,
	                                                     sizeof(*p),
	                                                     cleanup_single_query_request);

	*p = query_ctx;
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

	if (execute_query(ctx, &query_ctx->param)) {
		query_ctx->cleanup = true;
		send_service_unavailable_error(DB_REQ_ERROR, req);
	}

	return 0;
}

static int updates(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);
	return do_multiple_queries(true, false, req);
}

void cleanup_world_handlers(global_data_t *global_data)
{
	cache_destroy(&global_data->request_handler_data.world_cache);
}

void initialize_world_handlers(const config_t *config,
                               global_data_t *global_data,
                               h2o_hostconf_t *hostconf,
                               h2o_access_log_filehandle_t *log_handle)
{
	add_prepared_statement(WORLD_TABLE_NAME, WORLD_QUERY, &global_data->prepared_statements);
	register_request_handler("/db", single_query, hostconf, log_handle);
	register_request_handler("/queries", multiple_queries, hostconf, log_handle);
	register_request_handler("/updates", updates, hostconf, log_handle);

	if (!cache_create(config->thread_num,
	                  CACHE_CAPACITY,
	                  CACHE_DURATION,
	                  free_cache_entry,
	                  &global_data->request_handler_data.world_cache)) {
		add_postinitialization_task(populate_cache, NULL, &global_data->postinitialization_tasks);
		register_request_handler("/cached-worlds", cached_queries, hostconf, log_handle);
	}
}
