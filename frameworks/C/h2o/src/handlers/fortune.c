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
#include <mustache.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <postgresql/libpq-fe.h>
#include <string.h>

#include "database.h"
#include "error.h"
#include "fortune.h"
#include "global_data.h"
#include "list.h"
#include "request_handler.h"
#include "thread.h"
#include "utility.h"

#define FORTUNE_TABLE_NAME "Fortune"
#define FORTUNE_QUERY "SELECT * FROM " FORTUNE_TABLE_NAME ";"
#define ID_FIELD_NAME "id"
#define MAX_IOVEC 64
#define MESSAGE_FIELD_NAME "message"
#define NEW_FORTUNE_ID "0"
#define NEW_FORTUNE_MESSAGE "Additional fortune added at request time."
#define TEMPLATE_PATH_SUFFIX "/fortunes.mustache"

typedef struct {
	list_t l;
	h2o_iovec_t id;
	h2o_iovec_t message;
} fortune_t;

typedef struct {
	list_t l;
	size_t iovcnt;
	size_t max_iovcnt;
	h2o_iovec_t iov[];
} iovec_list_t;

typedef struct {
	PGresult *data;
	const fortune_t *fortune_iter;
	list_t *iovec_list;
	iovec_list_t *iovec_list_iter;
	h2o_req_t *req;
	list_t *result;
	size_t content_length;
	size_t num_result;
	bool cleanup;
	db_query_param_t param;
	h2o_generator_t generator;
} fortune_ctx_t;

typedef struct {
	FILE *input;
	const char *name;
} template_input_t;

static uintmax_t add_iovec(mustache_api_t *api,
                           void *userdata,
                           const char *buffer,
                           uintmax_t buffer_size);
static void cleanup_fortunes(fortune_ctx_t *fortune_ctx);
static void cleanup_request(void *data);
static int compare_fortunes(const list_t *x, const list_t *y);
static void complete_fortunes(struct st_h2o_generator_t *self, h2o_req_t *req);
static int fortunes(struct st_h2o_handler_t *self, h2o_req_t *req);
static void on_fortune_error(db_query_param_t *param, const char *error_string);
static result_return_t on_fortune_result(db_query_param_t *param, PGresult *result);
static uintmax_t on_fortune_section(mustache_api_t *api,
                                    void *userdata,
                                    mustache_token_section_t *token);
static void on_fortune_timeout(db_query_param_t *param);
static uintmax_t on_fortune_variable(mustache_api_t *api,
                                     void *userdata,
                                     mustache_token_variable_t *token);
static uintmax_t prerender_section(mustache_api_t *api,
                                   void *userdata,
                                   mustache_token_section_t *token);
static uintmax_t prerender_variable(mustache_api_t *api,
                                    void *userdata,
                                    mustache_token_variable_t *token);
static uintmax_t read_template(mustache_api_t *api,
                               void *userdata,
                               char *buffer,
                               uintmax_t buffer_size);
static void template_error(mustache_api_t *api,
                           void *userdata,
                           uintmax_t lineno,
                           const char *error);

static uintmax_t add_iovec(mustache_api_t *api,
                           void *userdata,
                           const char *buffer,
                           uintmax_t buffer_size)
{
	IGNORE_FUNCTION_PARAMETER(api);

	fortune_ctx_t * const fortune_ctx = userdata;
	iovec_list_t *iovec_list = fortune_ctx->iovec_list_iter;

	if (iovec_list->iovcnt >= iovec_list->max_iovcnt) {
		const size_t sz = offsetof(iovec_list_t, iov) + MAX_IOVEC * sizeof(h2o_iovec_t);

		iovec_list = h2o_mem_alloc_pool(&fortune_ctx->req->pool, sz);
		memset(iovec_list, 0, offsetof(iovec_list_t, iov));
		iovec_list->max_iovcnt = MAX_IOVEC;
		fortune_ctx->iovec_list_iter->l.next = &iovec_list->l;
		fortune_ctx->iovec_list_iter = iovec_list;
	}

	memset(iovec_list->iov + iovec_list->iovcnt, 0, sizeof(*iovec_list->iov));
	iovec_list->iov[iovec_list->iovcnt].base = (char *) buffer;
	iovec_list->iov[iovec_list->iovcnt++].len = buffer_size;
	fortune_ctx->content_length += buffer_size;
	return 1;
}

static void cleanup_fortunes(fortune_ctx_t *fortune_ctx)
{
	if (fortune_ctx->data)
		PQclear(fortune_ctx->data);

	free(fortune_ctx);
}

static void cleanup_request(void *data)
{
	fortune_ctx_t * const fortune_ctx = *(fortune_ctx_t **) data;

	if (fortune_ctx->cleanup)
		cleanup_fortunes(fortune_ctx);
	else
		fortune_ctx->cleanup = true;
}

static int compare_fortunes(const list_t *x, const list_t *y)
{
	const fortune_t * const f1 = H2O_STRUCT_FROM_MEMBER(fortune_t, l, x);
	const fortune_t * const f2 = H2O_STRUCT_FROM_MEMBER(fortune_t, l, y);
	const size_t sz = MIN(f1->message.len, f2->message.len);
	int ret = memcmp(f1->message.base, f2->message.base, sz);

	if (!ret)
		ret = f1->message.len < f2->message.len ? -1 : f1->message.len > f2->message.len;

	return ret;
}

static void complete_fortunes(struct st_h2o_generator_t *self, h2o_req_t *req)
{
	fortune_ctx_t * const fortune_ctx = H2O_STRUCT_FROM_MEMBER(fortune_ctx_t, generator, self);
	iovec_list_t * const iovec_list = H2O_STRUCT_FROM_MEMBER(iovec_list_t,
	                                                         l,
	                                                         fortune_ctx->iovec_list);

	fortune_ctx->iovec_list = iovec_list->l.next;

	const h2o_send_state_t state = fortune_ctx->iovec_list ?
	                               H2O_SEND_STATE_IN_PROGRESS :
	                               H2O_SEND_STATE_FINAL;

	h2o_send(req, iovec_list->iov, iovec_list->iovcnt, state);
}

static int fortunes(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);

	thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
	                                                      event_loop.h2o_ctx,
	                                                      req->conn->ctx);
	fortune_ctx_t * const fortune_ctx = h2o_mem_alloc(sizeof(*fortune_ctx));
	fortune_t * const fortune = h2o_mem_alloc_pool(&req->pool, sizeof(*fortune));
	fortune_ctx_t ** const p = h2o_mem_alloc_shared(&req->pool, sizeof(*p), cleanup_request);

	*p = fortune_ctx;
	memset(fortune, 0, sizeof(*fortune));
	fortune->id.base = NEW_FORTUNE_ID;
	fortune->id.len = sizeof(NEW_FORTUNE_ID) - 1;
	fortune->message.base = NEW_FORTUNE_MESSAGE;
	fortune->message.len = sizeof(NEW_FORTUNE_MESSAGE) - 1;
	memset(fortune_ctx, 0, sizeof(*fortune_ctx));
	fortune_ctx->generator.proceed = complete_fortunes;
	fortune_ctx->num_result = 1;
	fortune_ctx->param.command = FORTUNE_TABLE_NAME;
	fortune_ctx->param.on_error = on_fortune_error;
	fortune_ctx->param.on_result = on_fortune_result;
	fortune_ctx->param.on_timeout = on_fortune_timeout;
	fortune_ctx->param.flags = IS_PREPARED;
	fortune_ctx->req = req;
	fortune_ctx->result = &fortune->l;

	if (execute_query(ctx, &fortune_ctx->param)) {
		fortune_ctx->cleanup = true;
		send_service_unavailable_error(DB_REQ_ERROR, req);
	}

	return 0;
}

static void on_fortune_error(db_query_param_t *param, const char *error_string)
{
	fortune_ctx_t * const fortune_ctx = H2O_STRUCT_FROM_MEMBER(fortune_ctx_t, param, param);

	if (fortune_ctx->cleanup)
		cleanup_fortunes(fortune_ctx);
	else {
		fortune_ctx->cleanup = true;
		send_error(BAD_GATEWAY, error_string, fortune_ctx->req);
	}
}

static result_return_t on_fortune_result(db_query_param_t *param, PGresult *result)
{
	fortune_ctx_t * const fortune_ctx = H2O_STRUCT_FROM_MEMBER(fortune_ctx_t, param, param);
	const bool cleanup = fortune_ctx->cleanup;

	fortune_ctx->cleanup = true;
	fortune_ctx->data = result;

	if (cleanup)
		cleanup_fortunes(fortune_ctx);
	else if (PQresultStatus(result) == PGRES_TUPLES_OK) {
		assert(PQnfields(result) == 2);

		const size_t num_rows = PQntuples(result);

		for (size_t i = 0; i < num_rows; i++) {
			fortune_t * const fortune = h2o_mem_alloc_pool(&fortune_ctx->req->pool,
			                                               sizeof(*fortune));
			char * const id = PQgetvalue(result, i, 0);
			char * const message = PQgetvalue(result, i, 1);
			const size_t id_len = PQgetlength(result, i, 0);
			const size_t message_len = PQgetlength(result, i, 1);

			assert(id && id_len && isdigit(*id) && message);
			memset(fortune, 0, sizeof(*fortune));
			fortune->id.base = id;
			fortune->id.len = id_len;
			fortune->message = h2o_htmlescape(&fortune_ctx->req->pool,
			                                  message,
			                                  message_len);
			fortune->l.next = fortune_ctx->result;
			fortune_ctx->result = &fortune->l;
			fortune_ctx->num_result++;
		}

		mustache_api_t api = {.sectget = on_fortune_section,
		                      .varget = on_fortune_variable,
		                      .write = add_iovec};
		thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
		                                                      event_loop.h2o_ctx,
		                                                      fortune_ctx->req->conn->ctx);
		const size_t iovcnt = MIN(MAX_IOVEC, fortune_ctx->num_result * 5 + 2);
		const size_t sz = offsetof(iovec_list_t, iov) + iovcnt * sizeof(h2o_iovec_t);
		iovec_list_t * const iovec_list = h2o_mem_alloc_pool(&fortune_ctx->req->pool, sz);

		memset(iovec_list, 0, offsetof(iovec_list_t, iov));
		iovec_list->max_iovcnt = iovcnt;
		fortune_ctx->iovec_list_iter = iovec_list;
		fortune_ctx->result = sort_list(fortune_ctx->result, compare_fortunes);

		if (mustache_render(&api,
		                    fortune_ctx,
		                    ctx->global_data->request_handler_data.fortunes_template)) {
			fortune_ctx->iovec_list = iovec_list->l.next;
			set_default_response_param(HTML, fortune_ctx->content_length, fortune_ctx->req);
			h2o_start_response(fortune_ctx->req, &fortune_ctx->generator);

			const h2o_send_state_t state = fortune_ctx->iovec_list ?
			                               H2O_SEND_STATE_IN_PROGRESS :
			                               H2O_SEND_STATE_FINAL;

			h2o_send(fortune_ctx->req, iovec_list->iov, iovec_list->iovcnt, state);
		}
		else
			send_error(INTERNAL_SERVER_ERROR, REQ_ERROR, fortune_ctx->req);
	}
	else {
		LIBRARY_ERROR("PQresultStatus", PQresultErrorMessage(result));
		send_error(BAD_GATEWAY, DB_ERROR, fortune_ctx->req);
	}

	return DONE;
}

static uintmax_t on_fortune_section(mustache_api_t *api,
                                    void *userdata,
                                    mustache_token_section_t *token)
{
	fortune_ctx_t * const fortune_ctx = userdata;
	uintmax_t ret = 1;

	if (fortune_ctx->num_result) {
		assert(fortune_ctx->result);

		const list_t *iter = fortune_ctx->result;

		do {
			fortune_ctx->fortune_iter = H2O_STRUCT_FROM_MEMBER(fortune_t, l, iter);
			iter = iter->next;

			if (!mustache_render(api, fortune_ctx, token->section)) {
				ret = 0;
				break;
			}
		} while (iter);
	}

	return ret;
}

static void on_fortune_timeout(db_query_param_t *param)
{
	fortune_ctx_t * const fortune_ctx = H2O_STRUCT_FROM_MEMBER(fortune_ctx_t, param, param);

	if (fortune_ctx->cleanup)
		cleanup_fortunes(fortune_ctx);
	else {
		fortune_ctx->cleanup = true;
		send_error(GATEWAY_TIMEOUT, DB_TIMEOUT_ERROR, fortune_ctx->req);
	}
}

static uintmax_t on_fortune_variable(mustache_api_t *api,
                                     void *userdata,
                                     mustache_token_variable_t *token)
{
	fortune_ctx_t * const fortune_ctx = userdata;
	const h2o_iovec_t * const iovec = (const h2o_iovec_t *)
		((const char *) fortune_ctx->fortune_iter + (size_t) token->userdata);

	return add_iovec(api, fortune_ctx, iovec->base, iovec->len);
}

static uintmax_t prerender_section(mustache_api_t *api,
                                   void *userdata,
                                   mustache_token_section_t *token)
{
	bool * const in_section = userdata;
	uintmax_t ret = 0;

	if (!*in_section && !strcmp(token->name, FORTUNE_TABLE_NAME)) {
		*in_section = true;
		ret = mustache_prerender(api, userdata, token->section);
		*in_section = false;
	}

	return ret;
}

static uintmax_t prerender_variable(mustache_api_t *api,
                                    void *userdata,
                                    mustache_token_variable_t *token)
{
	IGNORE_FUNCTION_PARAMETER(api);

	bool * const in_section = userdata;
	uintmax_t ret = 0;

	if (*in_section) {
		if (token->text_length == sizeof(ID_FIELD_NAME) - 1 &&
		    !memcmp(token->text, ID_FIELD_NAME, sizeof(ID_FIELD_NAME) - 1)) {
			token->userdata = (void *) offsetof(fortune_t, id);
			ret = 1;
		}
		else if (token->text_length == sizeof(MESSAGE_FIELD_NAME) - 1 &&
		         !memcmp(token->text, MESSAGE_FIELD_NAME, sizeof(MESSAGE_FIELD_NAME) - 1)) {
			token->userdata = (void *) offsetof(fortune_t, message);
			ret = 1;
		}
	}

	return ret;
}

static uintmax_t read_template(mustache_api_t *api,
                               void *userdata,
                               char *buffer,
                               uintmax_t buffer_size)
{
	IGNORE_FUNCTION_PARAMETER(api);

	const template_input_t * const template_input = userdata;

	return fread(buffer, sizeof(*buffer), buffer_size, template_input->input);
}

static void template_error(mustache_api_t *api,
                           void *userdata,
                           uintmax_t lineno,
                           const char *error)
{
	IGNORE_FUNCTION_PARAMETER(api);

	const template_input_t * const template_input = userdata;

	print_error(template_input->name, lineno, "mustache_compile", error);
}

void cleanup_fortunes_handler(global_data_t *global_data)
{
	if (global_data->request_handler_data.fortunes_template) {
		mustache_api_t api = {.freedata = NULL};

		mustache_free(&api, global_data->request_handler_data.fortunes_template);
	}
}

void initialize_fortunes_handler(const config_t *config,
                                 global_data_t *global_data,
                                 h2o_hostconf_t *hostconf,
                                 h2o_access_log_filehandle_t *log_handle)
{
	mustache_template_t *template = NULL;
	const size_t template_path_prefix_len = config->template_path ? strlen(config->template_path) : 0;
	char path[template_path_prefix_len + sizeof(TEMPLATE_PATH_SUFFIX)];

	memcpy(path, config->template_path, template_path_prefix_len);
	memcpy(path + template_path_prefix_len, TEMPLATE_PATH_SUFFIX, sizeof(TEMPLATE_PATH_SUFFIX));

	template_input_t template_input = {.input = fopen(path, "rb"), .name = path};

	if (template_input.input) {
		mustache_api_t api = {.error = template_error,
		                      .read = read_template,
		                      .sectget = prerender_section,
		                      .varget = prerender_variable};
		bool in_section = false;

		template = mustache_compile(&api, &template_input);

		if (template && !mustache_prerender(&api, &in_section, template)) {
			mustache_free(&api, template);
			template = NULL;
		}

		fclose(template_input.input);
	}
	else
		STANDARD_ERROR("fopen");

	if (template) {
		global_data->request_handler_data.fortunes_template = template;
		add_prepared_statement(FORTUNE_TABLE_NAME,
		                       FORTUNE_QUERY,
		                       &global_data->prepared_statements);
		register_request_handler("/fortunes", fortunes, hostconf, log_handle);
	}
}
