/*
 Copyright (c) 2018 Anton Valentinov Kirilov

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

#ifndef CACHE_H_

#define CACHE_H_

#include <pthread.h>
#include <stdint.h>
#include <h2o/cache.h>

typedef struct {
	h2o_cache_t **cache;
	pthread_spinlock_t *cache_lock;
	size_t cache_num;
} cache_t;

int cache_create(size_t concurrency,
                 size_t capacity,
                 uint64_t duration,
                 void (*destroy_cb)(h2o_iovec_t value),
                 cache_t *cache);
void cache_destroy(cache_t *cache);
h2o_cache_ref_t *cache_fetch(cache_t *cache,
                             uint64_t now,
                             h2o_iovec_t key,
                             h2o_cache_hashcode_t keyhash);
void cache_release(cache_t *cache, h2o_cache_ref_t *ref, h2o_cache_hashcode_t keyhash);
int cache_set(uint64_t now,
              h2o_iovec_t key,
              h2o_cache_hashcode_t keyhash,
              h2o_iovec_t value,
              cache_t *cache);

#endif // CACHE_H_
