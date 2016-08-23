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
#include <stdalign.h>
#include <string.h>
#include <yajl/yajl_gen.h>

#include "error.h"
#include "fortune.h"
#include "request_handler.h"
#include "utility.h"
#include "world.h"

typedef struct {
	yajl_gen gen;
	h2o_generator_t h2o_generator;
} json_ctx_t;

static void cleanup_json_response(struct st_h2o_generator_t *self, h2o_req_t *req);
static void complete_json_response(struct st_h2o_generator_t *self, h2o_req_t *req);
static int json_serializer(struct st_h2o_handler_t *self, h2o_req_t *req);
static int plaintext(struct st_h2o_handler_t *self, h2o_req_t *req);
static const char *status_code_to_string(http_status_code_t status_code);

static void cleanup_json_response(struct st_h2o_generator_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(req);

	json_ctx_t * const json_ctx = H2O_STRUCT_FROM_MEMBER(json_ctx_t, h2o_generator, self);

	yajl_gen_free(json_ctx->gen);
}

static void complete_json_response(struct st_h2o_generator_t *self, h2o_req_t *req)
{
	h2o_send(req, NULL, 0, true);
	cleanup_json_response(self, req);
}

static int json_serializer(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);

	json_ctx_t * const json_ctx = h2o_mem_alloc_pool(&req->pool, sizeof(*json_ctx));

	if (json_ctx) {
		memset(json_ctx, 0, sizeof(*json_ctx));
		json_ctx->gen = get_json_generator(&req->pool);
		json_ctx->h2o_generator.proceed = complete_json_response;
		json_ctx->h2o_generator.stop = cleanup_json_response;

		if (json_ctx->gen) {
			CHECK_YAJL_STATUS(yajl_gen_map_open, json_ctx->gen);
			CHECK_YAJL_STATUS(yajl_gen_string, json_ctx->gen, YAJL_STRLIT("message"));
			CHECK_YAJL_STATUS(yajl_gen_string, json_ctx->gen, YAJL_STRLIT("Hello, World!"));
			CHECK_YAJL_STATUS(yajl_gen_map_close, json_ctx->gen);

			if (!send_json_response(json_ctx->gen, &json_ctx->h2o_generator, req))
				return 0;

error_yajl:
			yajl_gen_free(json_ctx->gen);
		}
	}

	send_error(INTERNAL_SERVER_ERROR, MEM_ALLOC_ERR_MSG, req);
	return 0;
}

static int plaintext(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);
	set_default_response_param(PLAIN, req);
	h2o_send_inline(req, H2O_STRLIT("Hello, World!"));
	return 0;
}

static const char *status_code_to_string(http_status_code_t status_code)
{
	const char *ret;

	switch (status_code) {
		case BAD_GATEWAY:
			ret = "Bad Gateway";
			break;
		case GATEWAY_TIMEOUT:
			ret = "Gateway Timeout";
			break;
		case INTERNAL_SERVER_ERROR:
			ret = "Internal Server Error";
			break;
		case OK:
			ret = "OK";
			break;
		case SERVICE_UNAVAILABLE:
			ret = "Service Unavailable";
			break;
		default:
			ret = "";
	}

	return ret;
}

const char *get_query_param(const char *query,
                            size_t query_len,
                            const char *param,
                            size_t param_len)
{
	const char *ret = NULL;

	while (param_len < query_len) {
		if (!memcmp(param, query, param_len)) {
			ret = query + param_len;
			break;
		}

		const char * const next = memchr(query, '&', query_len);

		if (!next)
			break;

		query_len -= next + 1 - query;
		query = next + 1;
	}

	return ret;
}

void register_request_handlers(h2o_hostconf_t *hostconf, h2o_access_log_filehandle_t *log_handle)
{
	h2o_pathconf_t *pathconf = h2o_config_register_path(hostconf, "/json", 0);
	h2o_handler_t *handler = h2o_create_handler(pathconf, sizeof(*handler));

	if (log_handle)
		h2o_access_log_register(pathconf, log_handle);

	handler->on_req = json_serializer;
	pathconf = h2o_config_register_path(hostconf, "/db", 0);
	handler = h2o_create_handler(pathconf, sizeof(*handler));
	handler->on_req = single_query;

	if (log_handle)
		h2o_access_log_register(pathconf, log_handle);

	pathconf = h2o_config_register_path(hostconf, "/queries", 0);
	handler = h2o_create_handler(pathconf, sizeof(*handler));
	handler->on_req = multiple_queries;

	if (log_handle)
		h2o_access_log_register(pathconf, log_handle);

	pathconf = h2o_config_register_path(hostconf, "/fortunes", 0);
	handler = h2o_create_handler(pathconf, sizeof(*handler));
	handler->on_req = fortunes;

	if (log_handle)
		h2o_access_log_register(pathconf, log_handle);

	pathconf = h2o_config_register_path(hostconf, "/updates", 0);
	handler = h2o_create_handler(pathconf, sizeof(*handler));
	handler->on_req = updates;

	if (log_handle)
		h2o_access_log_register(pathconf, log_handle);

	pathconf = h2o_config_register_path(hostconf, "/plaintext", 0);
	handler = h2o_create_handler(pathconf, sizeof(*handler));
	handler->on_req = plaintext;

	if (log_handle)
		h2o_access_log_register(pathconf, log_handle);
}

void send_error(http_status_code_t status_code, const char *body, h2o_req_t *req)
{
	h2o_send_error_generic(req, status_code, status_code_to_string(status_code), body, 0);
}

int send_json_response(yajl_gen gen, h2o_generator_t *h2o_generator, h2o_req_t *req)
{
	h2o_iovec_t h2o_iovec = {.len = 0};
	int ret = EXIT_FAILURE;

	static_assert(sizeof(h2o_iovec.base) == sizeof(const unsigned char *) &&
	              alignof(h2o_iovec.base) == alignof(const unsigned char *),
	              "Types must be compatible.");

	if (yajl_gen_get_buf(gen,
	                     (const unsigned char **) &h2o_iovec.base,
	                     &h2o_iovec.len) == yajl_gen_status_ok) {
		set_default_response_param(JSON, req);
		h2o_start_response(req, h2o_generator);
		h2o_send(req, &h2o_iovec, 1, false);
		ret = EXIT_SUCCESS;
	}

	return ret;
}

void send_service_unavailable_error(const char *body, h2o_req_t *req)
{
	h2o_add_header(&req->pool,
	               &req->res.headers,
	               H2O_TOKEN_RETRY_AFTER,
	               H2O_STRLIT(MKSTR(H2O_DEFAULT_HTTP1_REQ_TIMEOUT_IN_SECS)));
	h2o_send_error_503(req,
	                   status_code_to_string(SERVICE_UNAVAILABLE),
	                   body,
	                   H2O_SEND_ERROR_KEEP_HEADERS);
}

void set_default_response_param(content_type_t content_type, h2o_req_t *req)
{
	req->res.status = OK;
	req->res.reason = status_code_to_string(req->res.status);

	switch (content_type) {
		case JSON:
			h2o_add_header(&req->pool,
			               &req->res.headers,
			               H2O_TOKEN_CONTENT_TYPE,
			               H2O_STRLIT("application/json"));
			break;
		case PLAIN:
			h2o_add_header(&req->pool,
			               &req->res.headers,
			               H2O_TOKEN_CONTENT_TYPE,
			               H2O_STRLIT("text/plain"));
			break;
		default:
			h2o_add_header(&req->pool,
			               &req->res.headers,
			               H2O_TOKEN_CONTENT_TYPE,
			               H2O_STRLIT("text/html; charset=utf-8"));
	}
}
