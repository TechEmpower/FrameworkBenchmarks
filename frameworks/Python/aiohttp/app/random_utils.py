from random import randint

# Constants
# https://statmath.wu.ac.at/software/src/prng-3.0.2/doc/prng.html/Table_LCG.html
MAX_VALUE = 10000
MODULOS = 134217689
LCG_MULTIPLIER = 3162696
RANDOM_THRESHOLD = MODULOS - (MODULOS % (MAX_VALUE))

seed = randint(0, MAX_VALUE)

def random_id():
    """Generate a pseudo-random number based on a linear congruential generator"""
    global seed

    # Regenerate if we're above the threshold to avoid distribution bias
    while True:
        seed = (LCG_MULTIPLIER * seed) % MODULOS
        if seed < RANDOM_THRESHOLD:
            break

    return 1 + seed % MAX_VALUE

def random_unique_ids(n):
    """Generate n unique random IDs"""
    generated_ids = {random_id() for _ in range(n)}

    while len(generated_ids) < n:
        id_ = random_id()
        if id_ not in generated_ids:
            generated_ids.add(id_)

    return list(generated_ids)
