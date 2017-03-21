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
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <h2o/serverutil.h>
#include <sys/epoll.h>
#include <sys/syscall.h>

#include "database.h"
#include "error.h"
#include "event_loop.h"
#include "thread.h"

static void *run_thread(void *arg);

static void *run_thread(void *arg)
{
	thread_context_t ctx;

	initialize_thread_context(arg, false, &ctx);
	connect_to_database(&ctx);
	event_loop(&ctx);
	free_thread_context(&ctx);
	pthread_exit(NULL);
}

void free_thread_context(thread_context_t *ctx)
{
	free_database_state(ctx->event_loop.h2o_ctx.loop, &ctx->db_state);
	free_event_loop(&ctx->event_loop, &ctx->global_thread_data->h2o_receiver);
}

global_thread_data_t *initialize_global_thread_data(const config_t *config,
                                                    global_data_t *global_data)
{
	const size_t sz = config->thread_num * sizeof(thread_context_t);
	// The global thread data is modified only at program initialization and termination,
	// and is not accessed by performance-sensitive code, so false sharing is not a concern.
	global_thread_data_t * const ret = aligned_alloc(global_data->memory_alignment, sz);

	if (ret) {
		memset(ret, 0, sz);

		for (size_t i = 0; i < config->thread_num; i++) {
			ret[i].config = config;
			ret[i].global_data = global_data;
		}
	}

	return ret;
}

void initialize_thread_context(global_thread_data_t *global_thread_data,
                               bool is_main_thread,
                               thread_context_t *ctx)
{
	memset(ctx, 0, sizeof(*ctx));
	ctx->config = global_thread_data->config;
	ctx->global_data = global_thread_data->global_data;
	ctx->global_thread_data = global_thread_data;
	ctx->tid = syscall(SYS_gettid);
	ctx->random_seed = ctx->tid;
	initialize_event_loop(is_main_thread,
	                      global_thread_data->global_data,
	                      &global_thread_data->h2o_receiver,
	                      &ctx->event_loop);
	initialize_database_state(ctx->event_loop.h2o_ctx.loop, &ctx->db_state);
	global_thread_data->ctx = ctx;
}

void start_threads(global_thread_data_t *global_thread_data)
{
	const size_t num_cpus = h2o_numproc();

	// The first thread context is used by the main thread.
	global_thread_data->thread = pthread_self();

	for (size_t i = 1; i < global_thread_data->config->thread_num; i++)
		CHECK_ERROR(pthread_create,
		            &global_thread_data[i].thread,
		            NULL,
		            run_thread,
		            global_thread_data + i);

	// If the number of threads is not equal to the number of processors, then let the scheduler
	// decide how to balance the load.
	if (global_thread_data->config->thread_num == num_cpus) {
		const size_t cpusetsize = CPU_ALLOC_SIZE(num_cpus);
		cpu_set_t * const cpuset = CPU_ALLOC(num_cpus);

		if (!cpuset)
			abort();

		for (size_t i = 0; i < global_thread_data->config->thread_num; i++) {
			CPU_ZERO_S(cpusetsize, cpuset);
			CPU_SET_S(i, cpusetsize, cpuset);
			CHECK_ERROR(pthread_setaffinity_np, global_thread_data[i].thread, cpusetsize, cpuset);
		}

		CPU_FREE(cpuset);
	}
}
