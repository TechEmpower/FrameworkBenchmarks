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
#include <postgresql/libpq-fe.h>
#include <stddef.h>
#include <stdint.h>
#include <mustache.h>

#include "database.h"
#include "error.h"
#include "fortune.h"
#include "list.h"
#include "request_handler.h"
#include "thread.h"
#include "utility.h"

#define MAX_IOVEC 64
#define NEW_FORTUNE_ID "0"
#define NEW_FORTUNE_MESSAGE "Additional fortune added at request time."

typedef struct {
	list_t l;
	size_t iovcnt;
	size_t max_iovcnt;
	h2o_iovec_t iov[];
} iovec_list_t;

typedef struct {
	const fortune_t *fortune_iter;
	list_t *iovec_list;
	iovec_list_t *iovec_list_iter;
	h2o_req_t *req;
	list_t *result;
	size_t content_length;
	size_t num_result;
	db_query_param_t param;
	h2o_generator_t generator;
} fortune_ctx_t;

static uintmax_t add_iovec(mustache_api_t *api,
                           void *userdata,
                           const char *buffer,
                           uintmax_t buffer_size);
static void cleanup_fortunes(void *data);
static int compare_fortunes(const list_t *x, const list_t *y);
static void complete_fortunes(struct st_h2o_generator_t *self, h2o_req_t *req);
static list_t *get_sorted_sublist(list_t *head);
static list_t *merge_lists(list_t *head1, list_t *head2);
static void on_fortune_error(db_query_param_t *param, const char *error_string);
static result_return_t on_fortune_result(db_query_param_t *param, PGresult *result);
static uintmax_t on_fortune_section(mustache_api_t *api,
                                    void *userdata,
                                    mustache_token_section_t *token);
static void on_fortune_timeout(db_query_param_t *param);
static uintmax_t on_fortune_variable(mustache_api_t *api,
                                     void *userdata,
                                     mustache_token_variable_t *token);
static list_t *sort_fortunes(list_t *head);

static uintmax_t add_iovec(mustache_api_t *api,
                           void *userdata,
                           const char *buffer,
                           uintmax_t buffer_size)
{
	IGNORE_FUNCTION_PARAMETER(api);

	fortune_ctx_t * const fortune_ctx = userdata;
	iovec_list_t *iovec_list = fortune_ctx->iovec_list_iter;
	uintmax_t ret = 1;

	if (iovec_list->iovcnt >= iovec_list->max_iovcnt) {
		const size_t sz = offsetof(iovec_list_t, iov) + MAX_IOVEC * sizeof(h2o_iovec_t);

		iovec_list = h2o_mem_alloc_pool(&fortune_ctx->req->pool, sz);

		if (iovec_list) {
			memset(iovec_list, 0, offsetof(iovec_list_t, iov));
			iovec_list->max_iovcnt = MAX_IOVEC;
			fortune_ctx->iovec_list_iter->l.next = &iovec_list->l;
			fortune_ctx->iovec_list_iter = iovec_list;
		}
		else
			ret = 0;
	}

	if (ret) {
		memset(iovec_list->iov + iovec_list->iovcnt, 0, sizeof(*iovec_list->iov));
		iovec_list->iov[iovec_list->iovcnt].base = (char *) buffer;
		iovec_list->iov[iovec_list->iovcnt++].len = buffer_size;
		fortune_ctx->content_length += buffer_size;
	}

	return ret;
}

static void cleanup_fortunes(void *data)
{
	fortune_ctx_t * const fortune_ctx = data;
	const list_t *iter = fortune_ctx->result;

	if (iter)
		do {
			const fortune_t * const fortune = H2O_STRUCT_FROM_MEMBER(fortune_t, l, iter);

			if (fortune->data)
				PQclear(fortune->data);

			iter = iter->next;
		} while (iter);
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

static list_t *get_sorted_sublist(list_t *head)
{
	list_t *tail = head;

	if (head) {
		head = head->next;

		while (head && compare_fortunes(tail, head) <= 0) {
			tail = head;
			head = head->next;
		}
	}

	return tail;
}

static list_t *merge_lists(list_t *head1, list_t *head2)
{
	list_t *ret = NULL;
	list_t **current = &ret;

	while (1) {
		if (!head1) {
			*current = head2;
			break;
		}
		else if (!head2) {
			*current = head1;
			break;
		}
		// Checking for equality makes this algorithm a stable sort.
		else if (compare_fortunes(head1, head2) <= 0) {
			*current = head1;
			current = &head1->next;
			head1 = head1->next;
		}
		else {
			*current = head2;
			current = &head2->next;
			head2 = head2->next;
		}
	}

	return ret;
}

static void on_fortune_error(db_query_param_t *param, const char *error_string)
{
	fortune_ctx_t * const fortune_ctx = H2O_STRUCT_FROM_MEMBER(fortune_ctx_t, param, param);

	send_error(BAD_GATEWAY, error_string, fortune_ctx->req);
}

static result_return_t on_fortune_result(db_query_param_t *param, PGresult *result)
{
	fortune_ctx_t * const fortune_ctx = H2O_STRUCT_FROM_MEMBER(fortune_ctx_t, param, param);
	int ret = DONE;
	const ExecStatusType status = PQresultStatus(result);

	if (status == PGRES_TUPLES_OK) {
		const size_t num_rows = PQntuples(result);

		ret = SUCCESS;

		for (size_t i = 0; i < num_rows; i++) {
			fortune_t * const fortune = h2o_mem_alloc_pool(&fortune_ctx->req->pool,
			                                               sizeof(*fortune));

			if (fortune) {
				assert(PQnfields(result) == 2);

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

				if (!i)
					fortune->data = result;
			}
			else {
				send_error(INTERNAL_SERVER_ERROR, REQ_ERROR, fortune_ctx->req);
				ret = DONE;

				if (!i)
					PQclear(result);

				break;
			}
		}
	}
	else if (result) {
		LIBRARY_ERROR("PQresultStatus", PQresultErrorMessage(result));
		send_error(BAD_GATEWAY, DB_ERROR, fortune_ctx->req);
		PQclear(result);
	}
	else {
		mustache_api_t api = {.sectget = on_fortune_section,
		                      .varget = on_fortune_variable,
		                      .write = add_iovec};
		thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
		                                                      event_loop.h2o_ctx,
		                                                      fortune_ctx->req->conn->ctx);
		const size_t iovcnt = MIN(MAX_IOVEC, fortune_ctx->num_result * 5 + 2);
		const size_t sz = offsetof(iovec_list_t, iov) + iovcnt * sizeof(h2o_iovec_t);
		char _Alignas(iovec_list_t) mem[sz];
		iovec_list_t * const restrict iovec_list = (iovec_list_t *) mem;

		memset(iovec_list, 0, offsetof(iovec_list_t, iov));
		iovec_list->max_iovcnt = iovcnt;
		fortune_ctx->iovec_list_iter = iovec_list;
		fortune_ctx->result = sort_fortunes(fortune_ctx->result);

		if (mustache_render(&api, fortune_ctx, ctx->global_data->fortunes_template)) {
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

	return ret;
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

	send_error(GATEWAY_TIMEOUT, DB_TIMEOUT_ERROR, fortune_ctx->req);
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

// merge sort
static list_t *sort_fortunes(list_t *head)
{
	list_t **new_head;

	do {
		new_head = &head;

		for (list_t *iter = head; iter;) {
			list_t * const tail1 = get_sorted_sublist(iter);
			list_t * const head2 = tail1->next;

			if (!head2) {
				*new_head = iter;
				break;
			}

			list_t * const tail2 = get_sorted_sublist(head2);
			list_t * const head1 = iter;

			iter = tail2->next;
			tail1->next = NULL;
			tail2->next = NULL;
			*new_head = merge_lists(head1, head2);
			new_head = tail1->next ? &tail2->next : &tail1->next;
		}
	} while (new_head != &head);

	return head;
}

int fortunes(struct st_h2o_handler_t *self, h2o_req_t *req)
{
	IGNORE_FUNCTION_PARAMETER(self);

	thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
	                                                      event_loop.h2o_ctx,
	                                                      req->conn->ctx);
	fortune_ctx_t * const fortune_ctx = h2o_mem_alloc_shared(&req->pool,
	                                                         sizeof(*fortune_ctx),
	                                                         cleanup_fortunes);

	if (fortune_ctx) {
		fortune_t * const fortune = h2o_mem_alloc_pool(&req->pool, sizeof(*fortune));

		if (fortune) {
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

			if (execute_query(ctx, &fortune_ctx->param))
				send_service_unavailable_error(DB_REQ_ERROR, req);
		}
		else
			send_error(INTERNAL_SERVER_ERROR, REQ_ERROR, req);
	}
	else
		send_error(INTERNAL_SERVER_ERROR, REQ_ERROR, req);

	return 0;
}
