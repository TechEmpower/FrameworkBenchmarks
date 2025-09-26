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

#ifndef UTILITY_H_

#define UTILITY_H_

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <yajl/yajl_gen.h>

#include "list.h"

// Note that the parameter must be an actual array, and not an array that has decayed into a
// pointer, for example.
#define ARRAY_SIZE(a) (sizeof(a) / sizeof(*(a)))
// mainly used to silence compiler warnings about unused function parameters
#define IGNORE_FUNCTION_PARAMETER(p) ((void) (p))
// Do not use the following MAX and MIN macros with parameters that have side effects.
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define TOSTRING(x) # x
#define MKSTR(x) TOSTRING(x)
#define YAJL_STRLIT(s) (const unsigned char *) (s), sizeof(s) - 1

typedef struct {
	list_t l;
	yajl_gen gen;
} json_generator_t;

void free_json_generator(json_generator_t *gen, list_t **pool, size_t *gen_num, size_t max_gen);
yajl_gen_status gen_integer(long long number, char *buf, size_t len, yajl_gen gen);
json_generator_t *get_json_generator(list_t **pool, size_t *gen_num);
size_t get_maximum_cache_line_size(void);
uint32_t get_random_number(uint32_t max_rand, unsigned int *seed);
bool is_power_of_2(size_t x);
size_t round_up_to_power_of_2(size_t x);
// stable sort
list_t *sort_list(list_t *head, int (*compare)(const list_t *, const list_t *));

#endif // UTILITY_H_
