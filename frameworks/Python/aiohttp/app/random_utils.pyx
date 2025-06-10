# cython: boundscheck=False
# cython: wraparound=False
# cython: initializedcheck=False

from libc.stdlib cimport rand, srand, calloc, free
from libc.time cimport time

import cython

# Constants
cdef unsigned int MAX_VALUE = 10000
# https://statmath.wu.ac.at/software/src/prng-3.0.2/doc/prng.html/Table_LCG.html
cdef unsigned int MODULOS = 134217689
cdef unsigned int RANDOM_THRESHOLD = MODULOS - (MODULOS % MAX_VALUE)
cdef unsigned int LCG_MULTIPLIER = 3162696
cdef bint* seen = <bint*>calloc(MAX_VALUE + 1, sizeof(bint))  # Bit array simulation

cdef unsigned long seed = 0

cdef void _init_seed():
    global seed
    srand(<unsigned long>time(NULL))
    seed = rand() % MAX_VALUE

    if seed == 0:
        seed = 1  # Ensure seed is never zero to avoid low number cycle

_init_seed()

@cython.boundscheck(False)
@cython.wraparound(False)
cdef inline unsigned int _next_random() noexcept nogil:
    """Generate a pseudo-random number based on a linear congruential generator"""
    global seed

    cdef unsigned long next_val = (<unsigned long>LCG_MULTIPLIER * seed) % MODULOS

    while next_val >= RANDOM_THRESHOLD:
        next_val = (next_val * LCG_MULTIPLIER) % MODULOS

    seed = next_val
    return seed

@cython.boundscheck(False)
@cython.wraparound(False)
cpdef unsigned int random_id() noexcept nogil:
    """Generate a pseudo-random number in range [1, MAX_VALUE]"""
    return 1 + (_next_random() % MAX_VALUE)

@cython.boundscheck(False) 
@cython.wraparound(False)
cpdef list[unsigned int] random_unique_ids(int n):
    """Generate n unique random IDs in range[1, 10001]"""
    cdef list[int] result = [0] * n
    cdef int candidate, count = 0

    try:
        while count < n:
            candidate = random_id()

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