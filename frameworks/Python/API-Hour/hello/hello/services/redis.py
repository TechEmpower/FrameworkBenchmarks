import asyncio
from random import randint


@asyncio.coroutine
def get_random_record(container):
    idx = randint(1, 10000)
    random_number = yield from container.engines['redis'].get('world:%i' % idx)
    return {'Id': idx, 'RandomNumber': random_number}