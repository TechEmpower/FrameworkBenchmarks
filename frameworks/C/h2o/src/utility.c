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

#include <assert.h>
#include <h2o.h>
#include <stdalign.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <yajl/yajl_gen.h>

#include "utility.h"

static void mem_pool_free(void *ctx, void *ptr);
static void *mem_pool_malloc(void *ctx, size_t sz);
static void *mem_pool_realloc(void *ctx, void *ptr, size_t sz);

static void mem_pool_free(void *ctx, void *ptr)
{
	// The memory pool will free all allocations in one go.
	IGNORE_FUNCTION_PARAMETER(ctx);
	IGNORE_FUNCTION_PARAMETER(ptr);
}

static void *mem_pool_malloc(void *ctx, size_t sz)
{
	size_t * const p = h2o_mem_alloc_pool(ctx, sz + sizeof(*p));
	void * const ret = p + 1;

	*p = sz;
	// check alignment
	assert(!(((uintptr_t) ret) & (alignof(void *) - 1)));
	return ret;
}

static void *mem_pool_realloc(void *ctx, void *ptr, size_t sz)
{
	void *ret;

	if (ptr) {
		const size_t old_sz = ((const size_t *) ptr)[-1];

		if (sz > old_sz) {
			ret = mem_pool_malloc(ctx, sz);
			memcpy(ret, ptr, old_sz);
		}
		else
			ret = ptr;
	}
	else
		ret = mem_pool_malloc(ctx, sz);

	return ret;
}

yajl_gen get_json_generator(h2o_mem_pool_t *pool)
{
	const yajl_alloc_funcs mem_pool_alloc_funcs = {mem_pool_malloc,
	                                               mem_pool_realloc,
	                                               mem_pool_free,
	                                               pool};

	return yajl_gen_alloc(&mem_pool_alloc_funcs);
}

uint32_t get_random_number(uint32_t max_rand, unsigned int *seed)
{
	// In general, RAND_MAX + 1 is not a multiple of max_rand,
	// so rand_r() % max_rand would be biased.
	const unsigned bucket_size = (RAND_MAX + 1U) / max_rand;
	const unsigned unbiased_rand_max = bucket_size * max_rand;
	unsigned ret;

	do
		ret = rand_r(seed);
	while (ret >= unbiased_rand_max);

	return ret / bucket_size;
}
