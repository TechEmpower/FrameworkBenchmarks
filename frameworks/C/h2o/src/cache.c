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
#include <string.h>
#include <h2o/cache.h>

#include "cache.h"
#include "utility.h"

static h2o_cache_t *get_cache(cache_t *cache, h2o_cache_hashcode_t keyhash)
{
	assert(is_power_of_2(cache->cache_num));
	return cache->cache[keyhash & (cache->cache_num - 1)];
}

int cache_create(size_t concurrency,
                 size_t capacity,
                 uint64_t duration,
                 void (*destroy_cb)(h2o_iovec_t value),
                 cache_t *cache)
{
	int ret = EXIT_SUCCESS;

	memset(cache, 0, sizeof(*cache));
	// Rounding up to a power of 2 simplifies the calculations a little bit, and, as any increase in
	// the number of caches, potentially reduces thread contention.
	cache->cache_num = round_up_to_power_of_2(concurrency);
	cache->cache_num = MAX(cache->cache_num, 1);
	capacity = (capacity + cache->cache_num - 1) / cache->cache_num;
	cache->cache = malloc(cache->cache_num * sizeof(*cache->cache));

	if (cache->cache)
		for (size_t i = 0; i < cache->cache_num; i++) {
			cache->cache[i] = h2o_cache_create(H2O_CACHE_FLAG_MULTITHREADED,
			                                   capacity,
			                                   duration,
			                                   destroy_cb);

			if (!cache->cache[i]) {
				cache->cache_num = i;
				cache_destroy(cache);
				cache->cache = NULL;
				ret = EXIT_FAILURE;
				break;
			}
		}
	else
		ret = EXIT_FAILURE;

	return ret;
}

void cache_destroy(cache_t *cache)
{
	if (cache->cache) {
		for (size_t i = 0; i < cache->cache_num; i++)
			h2o_cache_destroy(cache->cache[i]);

		free(cache->cache);
	}
}

h2o_cache_ref_t *cache_fetch(cache_t *cache, uint64_t now, h2o_iovec_t key)
{
	const h2o_cache_hashcode_t keyhash = h2o_cache_calchash(key.base, key.len);

	return h2o_cache_fetch(get_cache(cache, keyhash), now, key, keyhash);
}

void cache_release(cache_t *cache, h2o_cache_ref_t *ref)
{
	h2o_cache_release(get_cache(cache, h2o_cache_calchash(ref->key.base, ref->key.len)), ref);
}

int cache_set(uint64_t now, h2o_iovec_t key, h2o_iovec_t value, cache_t *cache)
{
	const h2o_cache_hashcode_t keyhash = h2o_cache_calchash(key.base, key.len);

	return h2o_cache_set(get_cache(cache, keyhash), now, key, keyhash, value);
}
