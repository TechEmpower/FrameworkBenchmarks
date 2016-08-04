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
#include <fcntl.h>
#include <h2o.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/epoll.h>
#include <sys/syscall.h>

#include "error.h"
#include "event_loop.h"
#include "thread.h"
#include "utility.h"

static void accept_connection(h2o_socket_t *listener, const char *err);
static void do_epoll_wait(h2o_socket_t *epoll_sock, const char *err);
static void on_close_connection(void *data);
static void process_messages(h2o_multithread_receiver_t *receiver, h2o_linklist_t *messages);
static void shutdown_server(h2o_socket_t *listener, const char *err);

static void accept_connection(h2o_socket_t *listener, const char *err)
{
	if (!err) {
		thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
		                                                      event_loop,
		                                                      listener->data);

		if (!ctx->global_data->shutdown) {
			size_t accepted = 0;

			do {
				h2o_socket_t * const sock = h2o_evloop_socket_accept(listener);

				if (!sock)
					break;

				ctx->event_loop.conn_num++;
				sock->on_close.cb = on_close_connection;
				sock->on_close.data = &ctx->event_loop.conn_num;
				h2o_accept(&ctx->event_loop.h2o_accept_ctx, sock);
			} while (++accepted < ctx->global_data->config->max_accept);
		}
	}
}

static void do_epoll_wait(h2o_socket_t *epoll_sock, const char *err)
{
	if (!err) {
		thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
		                                                      event_loop,
		                                                      epoll_sock->data);
		const size_t num_event = ctx->db_state.db_conn_num - ctx->db_state.free_db_conn_num;
		int ready;
		struct epoll_event event[num_event];

		do
			ready = epoll_wait(ctx->event_loop.epoll_fd, event, num_event, 0);
		while (ready < 0 && errno == EINTR);

		if (ready > 0)
			for (size_t i = 0; i < (size_t) ready; i++) {
				void (** const on_write_ready)(void *) = event[i].data.ptr;

				(*on_write_ready)(on_write_ready);
			}
	}
}

static void on_close_connection(void *data)
{
	size_t * const conn_num = data;

	(*conn_num)--;
}

static void process_messages(h2o_multithread_receiver_t *receiver, h2o_linklist_t *messages)
{
	IGNORE_FUNCTION_PARAMETER(messages);

	thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
	                                                      event_loop.h2o_receiver,
	                                                      receiver);

	h2o_socket_read_stop(ctx->event_loop.h2o_socket);
}

static void shutdown_server(h2o_socket_t *listener, const char *err)
{
	if (!err) {
		thread_context_t * const ctx = H2O_STRUCT_FROM_MEMBER(thread_context_t,
		                                                      event_loop,
		                                                      listener->data);

		ctx->global_data->shutdown = true;
		h2o_socket_read_stop(ctx->event_loop.h2o_socket);

		for (size_t i = 1; i < ctx->global_data->config->thread_num; i++)
			h2o_multithread_send_message(&ctx[i].event_loop.h2o_receiver, NULL);
	}
}

void event_loop(thread_context_t *ctx)
{
	ctx->tid = syscall(SYS_gettid);
	ctx->random_seed = ctx->tid;

	while (!ctx->global_data->shutdown || ctx->event_loop.conn_num)
		h2o_evloop_run(ctx->event_loop.h2o_ctx.loop);
}

void free_event_loop(event_loop_t *event_loop)
{
	h2o_multithread_unregister_receiver(event_loop->h2o_ctx.queue, &event_loop->h2o_receiver);
	h2o_socket_close(event_loop->h2o_socket);
	h2o_socket_close(event_loop->epoll_socket);
	h2o_context_dispose(&event_loop->h2o_ctx);
}

void initialize_event_loop(bool is_main_thread,
                           global_data_t *global_data,
                           event_loop_t *loop)
{
	memset(loop, 0, sizeof(*loop));
	h2o_context_init(&loop->h2o_ctx, h2o_evloop_create(), &global_data->h2o_config);
	loop->h2o_accept_ctx.ctx = &loop->h2o_ctx;
	loop->h2o_accept_ctx.hosts = global_data->h2o_config.hosts;
	loop->h2o_accept_ctx.ssl_ctx = global_data->ssl_ctx;

	int listener_sd;

	if (is_main_thread)
		listener_sd = global_data->listener_sd;
	else {
		int flags;

		CHECK_ERRNO_RETURN(listener_sd, dup, global_data->listener_sd);
		CHECK_ERRNO_RETURN(flags, fcntl, listener_sd, F_GETFD);
		CHECK_ERRNO(fcntl, listener_sd, F_SETFD, flags | FD_CLOEXEC);
	}

	// Let all the threads race to call accept() on the socket; since the latter is
	// non-blocking, that will effectively act as load balancing.
	loop->h2o_socket = h2o_evloop_socket_create(loop->h2o_ctx.loop,
	                                            listener_sd,
	                                            H2O_SOCKET_FLAG_DONT_READ);
	loop->h2o_socket->data = loop;
	h2o_socket_read_start(loop->h2o_socket, accept_connection);
	h2o_multithread_register_receiver(loop->h2o_ctx.queue,
	                                  &loop->h2o_receiver,
	                                  process_messages);
	// libh2o's event loop does not support write polling unless it
	// controls sending the data as well, so do read polling on the
	// epoll file descriptor as a work-around.
	CHECK_ERRNO_RETURN(loop->epoll_fd, epoll_create1, EPOLL_CLOEXEC);
	loop->epoll_socket = h2o_evloop_socket_create(loop->h2o_ctx.loop,
	                                              loop->epoll_fd,
	                                              H2O_SOCKET_FLAG_DONT_READ);
	loop->epoll_socket->data = loop;
	h2o_socket_read_start(loop->epoll_socket, do_epoll_wait);

	if (is_main_thread) {
		global_data->signals = h2o_evloop_socket_create(loop->h2o_ctx.loop,
		                                                global_data->signal_fd,
		                                                H2O_SOCKET_FLAG_DONT_READ);
		global_data->signals->data = loop;
		h2o_socket_read_start(global_data->signals, shutdown_server);
	}
}

int start_write_polling(int fd,
                        void (**on_write_ready)(void *),
                        bool rearm,
                        event_loop_t *event_loop)
{
	struct epoll_event event;

	memset(&event, 0, sizeof(event));
	event.data.ptr = on_write_ready;
	event.events = EPOLLET | EPOLLONESHOT | EPOLLOUT | EPOLLRDHUP;
	return epoll_ctl(event_loop->epoll_fd, rearm ? EPOLL_CTL_MOD : EPOLL_CTL_ADD, fd, &event);
}

void stop_write_polling(int fd, event_loop_t *event_loop)
{
	epoll_ctl(event_loop->epoll_fd, EPOLL_CTL_DEL, fd, NULL);
}
