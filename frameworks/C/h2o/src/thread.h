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

#ifndef THREAD_H_

#define THREAD_H_

#include <assert.h>
#include <h2o.h>
#include <pthread.h>
#include <stdbool.h>
#include <sys/types.h>

#include "database.h"
#include "event_loop.h"
#include "utility.h"

typedef struct thread_context_t thread_context_t;

typedef struct global_thread_data_t {
	const config_t *config;
	thread_context_t *ctx;
	global_data_t *global_data;
	h2o_multithread_receiver_t h2o_receiver;
	pthread_t thread;
} global_thread_data_t;

struct thread_context_t {
	const config_t *config;
	global_data_t *global_data;
	// global_thread_data contains config and global_data as well,
	// but keep copies here to avoid some pointer chasing.
	global_thread_data_t *global_thread_data;
	list_t *json_generator;
	size_t json_generator_num;
	unsigned random_seed;
	pid_t tid;
	db_state_t db_state;
	event_loop_t event_loop;
};

void free_thread_context(thread_context_t *ctx);
global_thread_data_t *initialize_global_thread_data(const config_t *config,
                                                    global_data_t *global_data);
void initialize_thread_context(global_thread_data_t *global_thread_data,
                               bool is_main_thread,
                               thread_context_t *ctx);
void start_threads(global_thread_data_t *global_thread_data);

#endif // THREAD_H_
