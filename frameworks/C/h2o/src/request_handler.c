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

#include <h2o.h>
#include <stdbool.h>
#include <string.h>
#include <yajl/yajl_gen.h>

#include "global_data.h"
#include "request_handler.h"
#include "thread.h"
#include "utility.h"
#include "handlers/fortune.h"
#include "handlers/json_serializer.h"
#include "handlers/plaintext.h"
#include "handlers/request_handler_data.h"
#include "handlers/world.h"

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

void cleanup_request_handler_thread_data(request_handler_thread_data_t *data)
{
	cleanup_world_handler_thread_data(data);
}

void cleanup_request_handlers(request_handler_data_t *data)
{
	cleanup_fortunes_handler(data);
	cleanup_world_handlers(data);
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

void initialize_request_handler_thread_data(thread_context_t *ctx)
{
	const request_handler_data_t * const data =
		&ctx->global_thread_data->global_data->request_handler_data;

	initialize_world_handler_thread_data(ctx, data, &ctx->request_handler_data);
}

void initialize_request_handlers(const config_t *config,
                                 h2o_hostconf_t *hostconf,
                                 h2o_access_log_filehandle_t *log_handle,
                                 list_t **postinitialization_tasks,
                                 request_handler_data_t *data)
{
	IGNORE_FUNCTION_PARAMETER(postinitialization_tasks);
	initialize_fortunes_handler(config, hostconf, log_handle, data);
	initialize_json_serializer_handler(hostconf, log_handle);
	initialize_plaintext_handler(hostconf, log_handle);
	initialize_world_handlers(hostconf, log_handle, data);
}

void register_request_handler(const char *path,
                              int (*handler)(struct st_h2o_handler_t *, h2o_req_t *),
                              h2o_hostconf_t *hostconf,
                              h2o_access_log_filehandle_t *log_handle)
{
	h2o_pathconf_t * const pathconf = h2o_config_register_path(hostconf, path, 0);
	h2o_handler_t * const h = h2o_create_handler(pathconf, sizeof(*h));

	if (log_handle)
		h2o_access_log_register(pathconf, log_handle);

	h->on_req = handler;
}

void send_error(http_status_code_t status_code, const char *body, h2o_req_t *req)
{
	h2o_send_error_generic(req, status_code, status_code_to_string(status_code), body, 0);
}

int send_json_response(json_generator_t *gen, bool free_gen, h2o_req_t *req)
{
	const unsigned char *buf;
	size_t len;
	int ret = 1;

	if (yajl_gen_get_buf(gen->gen, &buf, &len) == yajl_gen_status_ok) {
		set_default_response_param(JSON, len, req);
		ret = 0;

		if (free_gen) {
			thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
			                                                      event_loop.h2o_ctx,
			                                                      req->conn->ctx);

			// h2o_send_inline() makes a copy of its input.
			h2o_send_inline(req, (char *) buf, len);
			free_json_generator(gen,
			                    &ctx->json_generator,
			                    &ctx->json_generator_num,
			                    ctx->global_thread_data->config->max_json_generator);
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
