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

#include <errno.h>
#include <h2o.h>
#include <inttypes.h>
#include <limits.h>
#include <netdb.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <openssl/ssl.h>
#include <sys/socket.h>
#include <sys/types.h>

#include "error.h"
#include "event_loop.h"
#include "global_data.h"
#include "thread.h"

#define DEFAULT_TCP_FASTOPEN_QUEUE_LEN 4096

static void accept_connection(h2o_socket_t *listener, const char *err);
static void accept_http_connection(h2o_socket_t *listener, const char *err);
static int get_listener_socket(const char *bind_address, uint16_t port);
static void on_close_connection(void *data);
static void process_messages(h2o_multithread_receiver_t *receiver, h2o_linklist_t *messages);
static void shutdown_server(h2o_socket_t *listener, const char *err);
static void start_accept_polling(const config_t *config,
                                 h2o_socket_cb accept_cb,
                                 bool is_https,
                                 event_loop_t *loop);

static void accept_connection(h2o_socket_t *listener, const char *err)
{
	if (err)
		ERROR(err);
	else {
		thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
		                                                      event_loop,
		                                                      listener->data);

		if (!ctx->shutdown) {
			size_t accepted = ctx->config->max_accept;

			assert(accepted);

			do {
				h2o_socket_t * const sock = h2o_evloop_socket_accept(listener);

				if (!sock)
					break;

				ctx->event_loop.conn_num++;
				sock->on_close.cb = on_close_connection;
				sock->on_close.data = &ctx->event_loop.conn_num;
				h2o_accept(&ctx->event_loop.h2o_accept_ctx, sock);
			} while (--accepted > 0);
		}
	}
}

static void accept_http_connection(h2o_socket_t *listener, const char *err)
{
	// Assume that err is most often NULL.
	thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
	                                                      event_loop,
	                                                      listener->data);
	SSL_CTX * const ssl_ctx = ctx->event_loop.h2o_accept_ctx.ssl_ctx;

	ctx->event_loop.h2o_accept_ctx.ssl_ctx = NULL;
	accept_connection(listener, err);
	ctx->event_loop.h2o_accept_ctx.ssl_ctx = ssl_ctx;
}

static int get_listener_socket(const char *bind_address, uint16_t port)
{
	int ret = -1;
	char buf[16];

	if ((size_t) snprintf(buf, sizeof(buf), "%" PRIu16, port) >= sizeof(buf)) {
		LIBRARY_ERROR("snprintf", "Truncated output.");
		return ret;
	}

	struct addrinfo *res = NULL;
	struct addrinfo hints = {.ai_socktype = SOCK_STREAM, .ai_flags = AI_PASSIVE};
	const int error_code = getaddrinfo(bind_address, buf, &hints, &res);

	if (error_code) {
		LIBRARY_ERROR("getaddrinfo", gai_strerror(error_code));
		return ret;
	}

	for (const struct addrinfo *iter = res; iter; iter = iter->ai_next) {
		const int s = socket(iter->ai_family,
		                     iter->ai_socktype | SOCK_NONBLOCK | SOCK_CLOEXEC,
		                     iter->ai_protocol);

		if (s == -1) {
			STANDARD_ERROR("socket");
			continue;
		}

#define LOCAL_CHECK_ERRNO(function, ...) \
	do { \
		const int error_code = (function)(__VA_ARGS__); \
		\
		if (error_code) { \
			print_library_error(__FILE__, __LINE__, #function, errno); \
			goto error; \
		} \
	} while(0)

		int option = 1;

		LOCAL_CHECK_ERRNO(setsockopt, s, SOL_SOCKET, SO_REUSEADDR, &option, sizeof(option));
		LOCAL_CHECK_ERRNO(setsockopt, s, SOL_SOCKET, SO_REUSEPORT, &option, sizeof(option));
		LOCAL_CHECK_ERRNO(setsockopt, s, IPPROTO_TCP, TCP_QUICKACK, &option, sizeof(option));
		option = H2O_DEFAULT_HANDSHAKE_TIMEOUT_IN_SECS;
		LOCAL_CHECK_ERRNO(setsockopt, s, IPPROTO_TCP, TCP_DEFER_ACCEPT, &option, sizeof(option));
		option = DEFAULT_TCP_FASTOPEN_QUEUE_LEN;
		LOCAL_CHECK_ERRNO(setsockopt, s, IPPROTO_TCP, TCP_FASTOPEN, &option, sizeof(option));
		LOCAL_CHECK_ERRNO(bind, s, iter->ai_addr, iter->ai_addrlen);
		LOCAL_CHECK_ERRNO(listen, s, INT_MAX);
		ret = s;
		break;

#undef LOCAL_CHECK_ERRNO

error:
		close(s);
	}

	freeaddrinfo(res);

	if (ret == -1)
		abort();

	return ret;
}

static void on_close_connection(void *data)
{
	size_t * const conn_num = data;

	(*conn_num)--;
}

static void process_messages(h2o_multithread_receiver_t *receiver, h2o_linklist_t *messages)
{
	global_thread_data_t * const global_thread_data = H2O_STRUCT_FROM_MEMBER(global_thread_data_t,
	                                                                         h2o_receiver,
	                                                                         receiver);

	while (!h2o_linklist_is_empty(messages)) {
		message_t * const msg = H2O_STRUCT_FROM_MEMBER(message_t, super, messages->next);

		h2o_linklist_unlink(&msg->super.link);

		switch (msg->type) {
			case SHUTDOWN:
				// Close the listening sockets immediately, so that if another instance of
				// the application is started before the current one exits (e.g. when doing
				// an update), it will accept all incoming connections.
				if (global_thread_data->ctx->event_loop.h2o_https_socket) {
					h2o_socket_read_stop(global_thread_data->ctx->event_loop.h2o_https_socket);
					h2o_socket_close(global_thread_data->ctx->event_loop.h2o_https_socket);
					global_thread_data->ctx->event_loop.h2o_https_socket = NULL;
				}

				if (global_thread_data->ctx->event_loop.h2o_socket) {
					h2o_socket_read_stop(global_thread_data->ctx->event_loop.h2o_socket);
					h2o_socket_close(global_thread_data->ctx->event_loop.h2o_socket);
					global_thread_data->ctx->event_loop.h2o_socket = NULL;
				}

				global_thread_data->ctx->shutdown = true;
				break;
			default:
				break;
		}

		free(msg);
	}
}

static void shutdown_server(h2o_socket_t *listener, const char *err)
{
	if (err)
		ERROR(err);
	else {
		thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
		                                                      event_loop,
		                                                      listener->data);

		ctx->shutdown = true;

		// Close the listening sockets immediately, so that if another instance
		// of the application is started before the current one exits (e.g. when
		// doing an update), it will accept all incoming connections.
		if (ctx->event_loop.h2o_https_socket) {
			h2o_socket_read_stop(ctx->event_loop.h2o_https_socket);
			h2o_socket_close(ctx->event_loop.h2o_https_socket);
			ctx->event_loop.h2o_https_socket = NULL;
		}

		if (ctx->event_loop.h2o_socket) {
			h2o_socket_read_stop(ctx->event_loop.h2o_socket);
			h2o_socket_close(ctx->event_loop.h2o_socket);
			ctx->event_loop.h2o_socket = NULL;
		}

		for (size_t i = ctx->config->thread_num - 1; i > 0; i--) {
			message_t * const msg = h2o_mem_alloc(sizeof(*msg));

			memset(msg, 0, sizeof(*msg));
			msg->type = SHUTDOWN;
			h2o_multithread_send_message(&ctx->global_thread_data[i].h2o_receiver, &msg->super);
		}
	}
}

static void start_accept_polling(const config_t *config,
                                 h2o_socket_cb accept_cb,
                                 bool is_https,
                                 event_loop_t *loop)
{
	const int listener_sd = get_listener_socket(config->bind_address,
	                                            is_https ? config->https_port : config->port);
	// Let all the threads race to call accept() on the socket; since the latter is
	// non-blocking, that will virtually act as load balancing, and SO_REUSEPORT
	// will make it efficient.
	h2o_socket_t * const h2o_socket = h2o_evloop_socket_create(loop->h2o_ctx.loop,
	                                                           listener_sd,
	                                                           H2O_SOCKET_FLAG_DONT_READ);

	if (is_https)
		loop->h2o_https_socket = h2o_socket;
	else
		loop->h2o_socket = h2o_socket;

	h2o_socket->data = loop;
	h2o_socket_read_start(h2o_socket, accept_cb);
}

void event_loop(thread_context_t *ctx)
{
	while (!ctx->shutdown || ctx->event_loop.conn_num)
		h2o_evloop_run(ctx->event_loop.h2o_ctx.loop, INT32_MAX);
}

void free_event_loop(event_loop_t *event_loop, h2o_multithread_receiver_t *h2o_receiver)
{
	if (event_loop->h2o_https_socket)
		h2o_socket_close(event_loop->h2o_https_socket);

	if (event_loop->h2o_socket)
		h2o_socket_close(event_loop->h2o_socket);

	h2o_multithread_unregister_receiver(event_loop->h2o_ctx.queue, h2o_receiver);

	h2o_loop_t * const loop = event_loop->h2o_ctx.loop;

	h2o_context_dispose(&event_loop->h2o_ctx);
	h2o_evloop_destroy(loop);
}

void initialize_event_loop(bool is_main_thread,
                           global_data_t *global_data,
                           h2o_multithread_receiver_t *h2o_receiver,
                           event_loop_t *loop)
{
	h2o_socket_cb accept_cb = accept_connection;
	const config_t * const config = global_data->global_thread_data->config;

	memset(loop, 0, sizeof(*loop));
	h2o_context_init(&loop->h2o_ctx, h2o_evloop_create(), &global_data->h2o_config);
	loop->h2o_accept_ctx.ctx = &loop->h2o_ctx;
	loop->h2o_accept_ctx.hosts = global_data->h2o_config.hosts;

	if (global_data->ssl_ctx) {
		loop->h2o_accept_ctx.ssl_ctx = global_data->ssl_ctx;
		start_accept_polling(config, accept_connection, true, loop);
		// Assume that the majority of the connections use HTTPS,
		// so HTTP can take a few extra operations.
		accept_cb = accept_http_connection;
	}

	start_accept_polling(config, accept_cb, false, loop);
	h2o_multithread_register_receiver(loop->h2o_ctx.queue,
	                                  h2o_receiver,
	                                  process_messages);

	if (is_main_thread) {
		global_data->signals = h2o_evloop_socket_create(loop->h2o_ctx.loop,
		                                                global_data->signal_fd,
		                                                H2O_SOCKET_FLAG_DONT_READ);
		global_data->signals->data = loop;
		h2o_socket_read_start(global_data->signals, shutdown_server);
	}
}
