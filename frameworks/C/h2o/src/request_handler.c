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
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <yajl/yajl_gen.h>

#include "error.h"
#include "fortune.h"
#include "request_handler.h"
#include "thread.h"
#include "utility.h"
#include "world.h"

#define HELLO_RESPONSE "Hello, World!"

static int json_serializer(struct st_h2o_handler_t *self, h2o_req_t *req);
static int plaintext(struct st_h2o_handler_t *self, h2o_req_t *req);
static const char *status_code_to_string(http_status_code_t status_code);

static int json_serializer(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);

	thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
	                                                      event_loop.h2o_ctx,
	                                                      req->conn->ctx);
	json_generator_t * const gen = get_json_generator(&ctx->json_generator,
	                                                  &ctx->json_generator_num);
	const struct {
		const char *message;
	} object = {HELLO_RESPONSE};

	if (gen) {
		CHECK_YAJL_STATUS(yajl_gen_map_open, gen->gen);
		CHECK_YAJL_STATUS(yajl_gen_string, gen->gen, YAJL_STRLIT("message"));
		CHECK_YAJL_STATUS(yajl_gen_string,
		                  gen->gen,
		                  (const unsigned char *) object.message,
		                  strlen(object.message));
		CHECK_YAJL_STATUS(yajl_gen_map_close, gen->gen);

		// The response is small enough, so that it is simpler to copy it
		// instead of doing a delayed deallocation of the JSON generator.
		if (!send_json_response(gen, true, req))
			return 0;

error_yajl:
		// If there is a problem with the generator, don't reuse it.
		free_json_generator(gen, NULL, NULL, 0);
	}

	send_error(INTERNAL_SERVER_ERROR, REQ_ERROR, req);
	return 0;
}

static int plaintext(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);

	h2o_generator_t generator;
	h2o_iovec_t body = {.base = HELLO_RESPONSE, .len = sizeof(HELLO_RESPONSE) - 1};

	memset(&generator, 0, sizeof(generator));
	set_default_response_param(PLAIN, sizeof(HELLO_RESPONSE) - 1, req);
	h2o_start_response(req, &generator);
	h2o_send(req, &body, 1, H2O_SEND_STATE_FINAL);
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

	pathconf = h2o_config_register_path(hostconf, "/cached-worlds", 0);
	handler = h2o_create_handler(pathconf, sizeof(*handler));
	handler->on_req = cached_queries;

	if (log_handle)
		h2o_access_log_register(pathconf, log_handle);
}

void send_error(http_status_code_t status_code, const char *body, h2o_req_t *req)
{
	h2o_send_error_generic(req, status_code, status_code_to_string(status_code), body, 0);
}

int send_json_response(json_generator_t *gen, bool free_gen, h2o_req_t *req)
{
	const unsigned char *buf;
	size_t len;
	int ret = EXIT_FAILURE;

	if (yajl_gen_get_buf(gen->gen, &buf, &len) == yajl_gen_status_ok) {
		set_default_response_param(JSON, len, req);
		ret = EXIT_SUCCESS;

		if (free_gen) {
			thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
			                                                      event_loop.h2o_ctx,
			                                                      req->conn->ctx);

			// h2o_send_inline() makes a copy of its input.
			h2o_send_inline(req, (char *) buf, len);
			free_json_generator(gen,
			                    &ctx->json_generator,
			                    &ctx->json_generator_num,
			                    ctx->config->max_json_generator);
		}
		else {
			h2o_generator_t generator;
			h2o_iovec_t body = {.base = (char *) buf, .len = len};

			memset(&generator, 0, sizeof(generator));
			h2o_start_response(req, &generator);
			h2o_send(req, &body, 1, H2O_SEND_STATE_FINAL);
		}
	}

	return ret;
}

void send_service_unavailable_error(const char *body, h2o_req_t *req)
{
	h2o_add_header(&req->pool,
	               &req->res.headers,
	               H2O_TOKEN_RETRY_AFTER,
	               NULL,
	               H2O_STRLIT(MKSTR(H2O_DEFAULT_HTTP1_REQ_TIMEOUT_IN_SECS)));
	h2o_send_error_503(req,
	                   status_code_to_string(SERVICE_UNAVAILABLE),
	                   body,
	                   H2O_SEND_ERROR_KEEP_HEADERS);
}

void set_default_response_param(content_type_t content_type, size_t content_length, h2o_req_t *req)
{
	req->res.content_length = content_length;
	req->res.status = OK;
	req->res.reason = status_code_to_string(req->res.status);

	switch (content_type) {
		case JSON:
			h2o_add_header(&req->pool,
			               &req->res.headers,
			               H2O_TOKEN_CONTENT_TYPE,
			               NULL,
			               H2O_STRLIT("application/json"));
			break;
		case PLAIN:
			h2o_add_header(&req->pool,
			               &req->res.headers,
			               H2O_TOKEN_CONTENT_TYPE,
			               NULL,
			               H2O_STRLIT("text/plain"));
			break;
		default:
			h2o_add_header(&req->pool,
			               &req->res.headers,
			               H2O_TOKEN_CONTENT_TYPE,
			               NULL,
			               H2O_STRLIT("text/html; charset=utf-8"));
	}
}
