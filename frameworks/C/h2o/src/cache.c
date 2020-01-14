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

#include <assert.h>
#include <pthread.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <h2o/cache.h>

#include "cache.h"
#include "error.h"
#include "utility.h"

// Increasing the number of caches by the following factor reduces contention; must be a power of 2.
#define CONCURRENCY_FACTOR 4

static size_t get_index(size_t n, h2o_cache_hashcode_t keyhash)
{
	assert(is_power_of_2(n));
	return keyhash & (n - 1);
}

int cache_create(size_t concurrency,
                 size_t capacity,
                 uint64_t duration,
                 void (*destroy_cb)(h2o_iovec_t value),
                 cache_t *cache)
{
	memset(cache, 0, sizeof(*cache));
	assert(is_power_of_2(CONCURRENCY_FACTOR));
	// Rounding up to a power of 2 simplifies the calculations a little bit, and, as any increase in
	// the number of caches, potentially reduces thread contention.
	cache->cache_num = CONCURRENCY_FACTOR * round_up_to_power_of_2(concurrency);
	cache->cache_num = MAX(cache->cache_num, 1);
	capacity = (capacity + cache->cache_num - 1) / cache->cache_num;
	cache->cache = malloc(cache->cache_num * sizeof(*cache->cache));

	if (!cache->cache)
		return 1;

	cache->cache_lock = malloc(cache->cache_num * sizeof(*cache->cache_lock));

	if (!cache->cache_lock)
		goto error;

	for (size_t i = 0; i < cache->cache_num; i++) {
		cache->cache[i] = h2o_cache_create(0, capacity, duration, destroy_cb);

		if (!cache->cache[i] || pthread_spin_init(cache->cache_lock + i, PTHREAD_PROCESS_PRIVATE)) {
			if (cache->cache[i])
				h2o_cache_destroy(cache->cache[i]);

			cache->cache_num = i;
			cache_destroy(cache);
			return 1;
		}
	}

	return 0;
error:
	free(cache->cache);
	return 1;
}

void cache_destroy(cache_t *cache)
{
	if (cache->cache) {
		assert(cache->cache_lock);

		for (size_t i = 0; i < cache->cache_num; i++) {
			h2o_cache_destroy(cache->cache[i]);
			pthread_spin_destroy(cache->cache_lock + i);
		}

		free(cache->cache);
		free((void *) cache->cache_lock);
		cache->cache = NULL;
		cache->cache_lock = NULL;
	}
	else
		assert(!cache->cache_lock);
}

h2o_cache_ref_t *cache_fetch(cache_t *cache,
                             uint64_t now,
                             h2o_iovec_t key,
                             h2o_cache_hashcode_t keyhash)
{
	if (!keyhash)
		keyhash = h2o_cache_calchash(key.base, key.len);

	const size_t idx = get_index(cache->cache_num, keyhash);
	pthread_spinlock_t * const lock = cache->cache_lock + idx;

	CHECK_ERROR(pthread_spin_lock, lock);

	h2o_cache_ref_t * const ret = h2o_cache_fetch(cache->cache[idx], now, key, keyhash);

	CHECK_ERROR(pthread_spin_unlock, lock);
	return ret;
}

void cache_release(cache_t *cache, h2o_cache_ref_t *ref, h2o_cache_hashcode_t keyhash)
{
	if (!keyhash)
		keyhash = h2o_cache_calchash(ref->key.base, ref->key.len);

	const size_t idx = get_index(cache->cache_num, keyhash);

	h2o_cache_release(cache->cache[idx], ref);
}

int cache_set(uint64_t now,
              h2o_iovec_t key,
              h2o_cache_hashcode_t keyhash,
              h2o_iovec_t value,
              cache_t *cache)
{
	if (!keyhash)
		keyhash = h2o_cache_calchash(key.base, key.len);

	const size_t idx = get_index(cache->cache_num, keyhash);
	pthread_spinlock_t * const lock = cache->cache_lock + idx;

	CHECK_ERROR(pthread_spin_lock, lock);

	const int ret = h2o_cache_set(cache->cache[idx], now, key, keyhash, value);

	CHECK_ERROR(pthread_spin_unlock, lock);
	return ret;
}
