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

#ifndef BITSET_H_

#define BITSET_H_

#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>

#include "utility.h"

typedef uint_fast32_t bitset_base_t;

#define BITSET_ISSET(i, b) bitset_isset((i), (b), sizeof(b) * CHAR_BIT)
#define BITSET_SET(i, b) bitset_set((i), (b), sizeof(b) * CHAR_BIT)
// Use a designated initializer to set all array elements to zero.
#define DEFINE_BITSET(b, s) \
	bitset_base_t (b)[ \
		((s) + sizeof(bitset_base_t) * CHAR_BIT - 1) / (sizeof(bitset_base_t) * CHAR_BIT)] = \
		{[0] = 0}

static inline bool bitset_isset(size_t i, bitset_base_t *b, size_t num)
{
	assert(i < num);

	IGNORE_FUNCTION_PARAMETER(num);

	return (b[i / (sizeof(*b) * CHAR_BIT)] >> (i % (sizeof(*b) * CHAR_BIT))) & (bitset_base_t) 1;
}

static inline void bitset_set(size_t i, bitset_base_t *b, size_t num)
{
	assert(i < num);

	IGNORE_FUNCTION_PARAMETER(num);

	const bitset_base_t mask = ((bitset_base_t) 1) << (i % (sizeof(*b) * CHAR_BIT));

	b[i / (sizeof(*b) * CHAR_BIT)] |= mask;
}

#endif // BITSET_H_
