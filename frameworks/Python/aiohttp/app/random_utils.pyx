# cython: boundscheck=False
# cython: wraparound=False
# cython: initializedcheck=False
# cython: cdivision=True

from libc.stdlib cimport calloc, free

import random

# Constants
cdef const unsigned short MAX_VALUE = 10000
# https://statmath.wu.ac.at/software/src/prng-3.0.2/doc/prng.html/Table_LCG.html
cdef const unsigned int MODULOS = 134217689
cdef const unsigned long LCG_MULTIPLIER = 3162696

cdef unsigned int RANDOM_THRESHOLD = MODULOS - (MODULOS % MAX_VALUE)
cdef char* seen = <char*>calloc(MAX_VALUE + 1, sizeof(char))  # Bit array simulation

cdef unsigned int seed = random.randint(1, MODULOS-1)

cdef inline unsigned int _next_random() noexcept nogil:
    """Generate a pseudo-random number based on a linear congruential generator"""
    global seed

    cdef unsigned int next_val = (LCG_MULTIPLIER * seed) % MODULOS

    while next_val >= RANDOM_THRESHOLD:
        next_val = (LCG_MULTIPLIER * next_val) % MODULOS

    seed = next_val
    return seed

cpdef unsigned short random_id() noexcept nogil:
    """Generate a pseudo-random number in range [1, MAX_VALUE]"""
    return 1 + (_next_random() % MAX_VALUE)

cpdef list[unsigned short] random_unique_ids(unsigned short n):
    """Generate n unique random IDs in range[1, 10001]"""
    cdef list[unsigned short] result = [0] * n
    cdef unsigned short candidate, count = 0

    try:
        while count < n:
            candidate = 1 + (_next_random() % MAX_VALUE)

            if seen[candidate] == 0:  # Not seen before
                seen[candidate] = 1
                result[count] = candidate
                count += 1
    finally:
        for i in result:
            seen[i] = 0

    return result

def _cleanup():
    global seen
    if seen != NULL:
        free(seen)
        seen = NULL

import atexit
atexit.register(_cleanup)