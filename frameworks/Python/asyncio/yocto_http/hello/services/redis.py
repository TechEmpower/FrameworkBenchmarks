from operator import itemgetter
import asyncio
from random import randint


@asyncio.coroutine
def get_random_record(container):
    idx = randint(1, 10000)
    random_number = yield from container.engines['redis'].get('world:%i' % idx)
    return {'Id': idx, 'RandomNumber': random_number}

@asyncio.coroutine
def get_random_records(container, limit):
    results = []
    for i in range(limit):
        idx = randint(1, 10000)
        random_number = yield from container.engines['redis'].get('world:%i' % idx)
        results.append({'Id': idx, 'RandomNumber': random_number})

    return results

@asyncio.coroutine
def update_random_records(container, limit):
    results = []
    for i in range(limit):
        idx = randint(1, 10000)
        random_number = yield from container.engines['redis'].get('world:%i' % idx)
        yield from container.engines['redis'].set('world:%i' % idx, str(randint(1, 10000)))
        results.append({'Id': idx, 'RandomNumber': random_number})
    return results

@asyncio.coroutine
def get_fortunes(container):
    results = []
    list_reply = yield from container.engines['redis'].lrange('fortunes')
    fortunes = yield from list_reply.aslist()
    i = 1
    for fortune in fortunes:
        results.append({'id': i, 'message': fortune})
        i += 1

    results.append({'id': 0, 'message': 'Additional fortune added at request time.'})
    results.sort(key=itemgetter('message'))

    return results