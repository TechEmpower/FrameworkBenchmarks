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
#include <sys/types.h>

#include "database.h"
#include "event_loop.h"
#include "utility.h"

#define DEFAULT_CACHE_LINE_SIZE 64

typedef struct thread_context_t thread_context_t;

struct thread_context_t {
	global_data_t *global_data;
	unsigned random_seed;
	pid_t tid;
	db_state_t db_state;
	event_loop_t event_loop;
	pthread_t thread;
	// Align on the cache line size to prevent false sharing.
	char padding[49];
};

static_assert(!(sizeof(thread_context_t) % DEFAULT_CACHE_LINE_SIZE),
              "The size of the thread_context_t structure must be a "
              "multiple of the cache line size.");

void free_thread_contexts(global_data_t *global_data);
thread_context_t *initialize_thread_contexts(global_data_t *global_data);
void start_threads(thread_context_t *ctx);

#endif // THREAD_H_
