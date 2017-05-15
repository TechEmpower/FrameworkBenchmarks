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
#include <stdint.h>
#include <stdlib.h>
#include <yajl/yajl_gen.h>

#include "list.h"
#include "utility.h"

void free_json_generator(json_generator_t *gen, list_t **pool, size_t *gen_num, size_t max_gen)
{
	if (gen) {
		assert(!pool || gen_num);

		if (pool && *gen_num < max_gen) {
			yajl_gen_reset(gen->gen, NULL);
			yajl_gen_clear(gen->gen);
			gen->l.next = *pool;
			*pool = &gen->l;
			(*gen_num)++;
		}
		else {
			yajl_gen_free(gen->gen);
			free(gen);
		}
	}
}

json_generator_t *get_json_generator(list_t **pool, size_t *gen_num)
{
	json_generator_t *ret;

	if (pool && *pool) {
		assert(gen_num && *gen_num);
		ret = H2O_STRUCT_FROM_MEMBER(json_generator_t, l, *pool);
		*pool = ret->l.next;
		(*gen_num)--;
	}
	else {
		ret = malloc(sizeof(*ret));

		if (ret) {
			ret->gen = yajl_gen_alloc(NULL);

			if (!ret->gen) {
				free(ret);
				ret = NULL;
			}
		}
	}

	return ret;
}

uint32_t get_random_number(uint32_t max_rand, unsigned int *seed)
{
	assert(max_rand <= (uint32_t) RAND_MAX);

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
