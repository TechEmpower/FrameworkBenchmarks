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
#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <yajl/yajl_gen.h>

#include "error.h"
#include "list.h"
#include "utility.h"

#define DEFAULT_CACHE_LINE_SIZE 128

static list_t *get_sorted_sublist(list_t *head, int (*compare)(const list_t *, const list_t *));
static list_t *merge_lists(list_t *head1,
                           list_t *head2,
                           int (*compare)(const list_t *, const list_t *));

static list_t *get_sorted_sublist(list_t *head, int (*compare)(const list_t *, const list_t *))
{
	list_t *tail = head;

	if (head) {
		head = head->next;

		while (head && compare(tail, head) <= 0) {
			tail = head;
			head = head->next;
		}
	}

	return tail;
}

static list_t *merge_lists(list_t *head1,
                           list_t *head2,
                           int (*compare)(const list_t *, const list_t *))
{
	list_t *ret = NULL;
	list_t **current = &ret;

	while (1) {
		if (!head1) {
			*current = head2;
			break;
		}
		else if (!head2) {
			*current = head1;
			break;
		}
		// Checking for equality makes this algorithm a stable sort.
		else if (compare(head1, head2) <= 0) {
			*current = head1;
			current = &head1->next;
			head1 = head1->next;
		}
		else {
			*current = head2;
			current = &head2->next;
			head2 = head2->next;
		}
	}

	return ret;
}

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

yajl_gen_status gen_integer(long long number, char *buf, size_t len, yajl_gen gen)
{
	if (!len)
		return yajl_gen_invalid_number;
	else if (number == LLONG_MIN) {
		const size_t l = snprintf(buf, len, "%lld", number);

		if (l >= len)
			return yajl_gen_invalid_number;

		len = l;
	}
	else {
		char *iter = buf + len;
		const bool negative = number < 0;

		if (negative) {
			number = -number;
			buf++;
		}

		do {
			if (--iter > buf) {
				*iter = '0' + number % 10;
				number /= 10;
			}
			else if (number < 10) {
				*iter = '0' + number;
				number = 0;
			}
			else
				return yajl_gen_invalid_number;
		} while (number);

		if (negative) {
			*--iter = '-';
			buf--;
		}

		len = buf + len - iter;
		buf = iter;
	}

	return yajl_gen_number(gen, buf, len);
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
		ret = h2o_mem_alloc(sizeof(*ret));
		memset(ret, 0, sizeof(*ret));
		ret->gen = yajl_gen_alloc(NULL);

		if (!ret->gen) {
			free(ret);
			ret = NULL;
		}
	}

	return ret;
}

size_t get_maximum_cache_line_size(void)
{
	const int name[] = {_SC_LEVEL1_DCACHE_LINESIZE,
	                    _SC_LEVEL2_CACHE_LINESIZE,
	                    _SC_LEVEL3_CACHE_LINESIZE,
	                    _SC_LEVEL4_CACHE_LINESIZE};
	size_t ret = 0;

	for (size_t i = 0; i < ARRAY_SIZE(name); i++) {
		errno = 0;

		const long rc = sysconf(name[i]);

		if (rc < 0) {
			if (errno)
				STANDARD_ERROR("sysconf");
		}
		else if ((size_t) rc > ret)
			ret = rc;
	}

	if (!ret)
		ret = DEFAULT_CACHE_LINE_SIZE;

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

bool is_power_of_2(size_t x)
{
	return !!x & !(x & (x - 1));
}

size_t round_up_to_power_of_2(size_t x)
{
	static_assert(sizeof(size_t) == sizeof(unsigned long),
	              "The size_t type must have the same size as unsigned long.");

	size_t ret = (SIZE_MAX ^ SIZE_MAX >> 1) >> __builtin_clzl(x);

	if (x - ret)
		ret <<= 1;

	return ret;
}

// merge sort
list_t *sort_list(list_t *head, int (*compare)(const list_t *, const list_t *))
{
	list_t **new_head;

	do {
		new_head = &head;

		for (list_t *iter = head; iter;) {
			list_t * const tail1 = get_sorted_sublist(iter, compare);
			list_t * const head2 = tail1->next;

			if (!head2) {
				*new_head = iter;
				break;
			}

			list_t * const tail2 = get_sorted_sublist(head2, compare);
			list_t * const head1 = iter;

			iter = tail2->next;
			tail1->next = NULL;
			tail2->next = NULL;
			*new_head = merge_lists(head1, head2, compare);
			new_head = tail1->next ? &tail2->next : &tail1->next;
		}
	} while (new_head != &head);

	return head;
}
