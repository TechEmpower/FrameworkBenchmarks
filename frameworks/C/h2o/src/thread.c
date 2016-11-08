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

#define _GNU_SOURCE

#include <errno.h>
#include <h2o.h>
#include <pthread.h>
#include <stdlib.h>
#include <h2o/serverutil.h>
#include <sys/epoll.h>

#include "database.h"
#include "error.h"
#include "event_loop.h"
#include "thread.h"

static void *run_thread(void *arg);

static void *run_thread(void *arg)
{
	connect_to_database(arg);
	event_loop(arg);
	pthread_exit(NULL);
}

void free_thread_contexts(global_data_t *global_data)
{
	thread_context_t * const ctx = global_data->ctx;

	for (size_t i = 0; i < ctx->global_data->config->thread_num; i++) {
		if (i)
			CHECK_ERROR(pthread_join, ctx[i].thread, NULL);
		else
			// Even though this is global data, we need to close
			// it before the associated event loop is cleaned up.
			h2o_socket_close(global_data->signals);

		free_database_state(ctx[i].event_loop.h2o_ctx.loop, &ctx[i].db_state);
		free_event_loop(&ctx[i].event_loop);
	}

	free(ctx);
}

thread_context_t *initialize_thread_contexts(global_data_t *global_data)
{
	const size_t sz = global_data->config->thread_num * sizeof(thread_context_t);
	thread_context_t * const ret = aligned_alloc(global_data->memory_alignment, sz);

	if (ret) {
		memset(ret, 0, sz);

		for (size_t i = 0; i < global_data->config->thread_num; i++) {
			ret[i].global_data = global_data;
			initialize_event_loop(!i, global_data, &ret[i].event_loop);
			initialize_database_state(ret[i].event_loop.h2o_ctx.loop, &ret[i].db_state);
		}
	}

	return ret;
}

void start_threads(thread_context_t *ctx)
{
	const size_t num_cpus = h2o_numproc();

	// The first thread context is used by the main thread.
	ctx->thread = pthread_self();

	for (size_t i = 1; i < ctx->global_data->config->thread_num; i++)
		CHECK_ERROR(pthread_create, &ctx[i].thread, NULL, run_thread, ctx + i);

	// If the number of threads is not equal to the number of processors, then let the scheduler
	// decide how to balance the load.
	if (ctx->global_data->config->thread_num == num_cpus) {
		const size_t cpusetsize = CPU_ALLOC_SIZE(num_cpus);
		cpu_set_t * const cpuset = CPU_ALLOC(num_cpus);

		if (!cpuset)
			abort();

		for (size_t i = 0; i < ctx->global_data->config->thread_num; i++) {
			CPU_ZERO_S(cpusetsize, cpuset);
			CPU_SET_S(i, cpusetsize, cpuset);
			CHECK_ERROR(pthread_setaffinity_np, ctx[i].thread, cpusetsize, cpuset);
		}

		CPU_FREE(cpuset);
	}
}
