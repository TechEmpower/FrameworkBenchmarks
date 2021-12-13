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

#include <h2o.h>
#include <limits.h>
#include <numaif.h>
#include <pthread.h>
#include <sched.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <h2o/serverutil.h>
#include <sys/syscall.h>

#include "database.h"
#include "error.h"
#include "event_loop.h"
#include "global_data.h"
#include "request_handler.h"
#include "thread.h"

static void *run_thread(void *arg);
static void set_thread_memory_allocation_policy(size_t thread_num);

static void *run_thread(void *arg)
{
	thread_context_t ctx;

	initialize_thread_context(arg, false, &ctx);
	set_thread_memory_allocation_policy(ctx.config->thread_num);
	event_loop(&ctx);
	free_thread_context(&ctx);
	pthread_exit(NULL);
}

static void set_thread_memory_allocation_policy(size_t thread_num)
{
	// There is no need to set a memory allocation policy unless
	// the application controls the processor affinity as well.
	if (thread_num % h2o_numproc())
		return;

	void *stack_addr;
	size_t stack_size;
	unsigned memory_node;
	pthread_attr_t attr;

	CHECK_ERRNO(syscall, SYS_getcpu, NULL, &memory_node, NULL);
	CHECK_ERROR(pthread_getattr_np, pthread_self(), &attr);
	CHECK_ERROR(pthread_attr_getstack, &attr, &stack_addr, &stack_size);
	pthread_attr_destroy(&attr);

	unsigned long nodemask[
		(memory_node + sizeof(unsigned long) * CHAR_BIT) / (sizeof(unsigned long) * CHAR_BIT)];

	memset(nodemask, 0, sizeof(nodemask));
	nodemask[memory_node / (sizeof(*nodemask) * CHAR_BIT)] |=
		1UL << (memory_node % (sizeof(*nodemask) * CHAR_BIT));
	CHECK_ERRNO(mbind,
	            stack_addr,
	            stack_size,
	            MPOL_PREFERRED,
	            nodemask,
	            memory_node + 1,
	            MPOL_MF_MOVE | MPOL_MF_STRICT);
	CHECK_ERRNO(set_mempolicy, MPOL_PREFERRED, NULL, 0);
}

void free_thread_context(thread_context_t *ctx)
{
	free_database_state(ctx->event_loop.h2o_ctx.loop, &ctx->db_state);
	free_event_loop(&ctx->event_loop, &ctx->global_thread_data->h2o_receiver);
	free_request_handler_thread_data(&ctx->request_handler_data);

	if (ctx->json_generator)
		do {
			json_generator_t * const gen = H2O_STRUCT_FROM_MEMBER(json_generator_t,
			                                                      l,
			                                                      ctx->json_generator);

			ctx->json_generator = gen->l.next;
			free_json_generator(gen, NULL, NULL, 0);
		} while (ctx->json_generator);
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
	else
		STANDARD_ERROR("aligned_alloc");

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
	ctx->random_seed = syscall(SYS_gettid);
	initialize_event_loop(is_main_thread,
	                      global_thread_data->global_data,
	                      &global_thread_data->h2o_receiver,
	                      &ctx->event_loop);
	initialize_database_state(ctx->event_loop.h2o_ctx.loop, &ctx->db_state);
	initialize_request_handler_thread_data(ctx->config, &ctx->request_handler_data);
	global_thread_data->ctx = ctx;
}

void start_threads(global_thread_data_t *global_thread_data)
{
	pthread_attr_t attr;
	const size_t num_cpus = h2o_numproc();
	const size_t cpusetsize = CPU_ALLOC_SIZE(num_cpus);
	cpu_set_t * const cpuset = CPU_ALLOC(num_cpus);

	if (!cpuset) {
		STANDARD_ERROR("CPU_ALLOC");
		abort();
	}

	CHECK_ERROR(pthread_attr_init, &attr);
	// The first thread context is used by the main thread.
	global_thread_data->thread = pthread_self();

	// If the number of threads is not a multiple of the number of processors, then
	// let the scheduler decide how to balance the load.
	if (global_thread_data->config->thread_num % num_cpus == 0) {
		CPU_ZERO_S(cpusetsize, cpuset);
		CPU_SET_S(0, cpusetsize, cpuset);
		CHECK_ERROR(pthread_setaffinity_np, global_thread_data->thread, cpusetsize, cpuset);
	}

	for (size_t i = global_thread_data->config->thread_num - 1; i > 0; i--) {
		if (global_thread_data->config->thread_num % num_cpus == 0) {
			CPU_ZERO_S(cpusetsize, cpuset);
			CPU_SET_S(i % num_cpus, cpusetsize, cpuset);
			CHECK_ERROR(pthread_attr_setaffinity_np, &attr, cpusetsize, cpuset);
		}

		CHECK_ERROR(pthread_create,
		            &global_thread_data[i].thread,
		            &attr,
		            run_thread,
		            global_thread_data + i);
	}

	pthread_attr_destroy(&attr);
	CPU_FREE(cpuset);
}
