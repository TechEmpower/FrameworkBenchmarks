import asyncio
import sys
from random import randint
from operator import itemgetter

@asyncio.coroutine
def get_random_record(container):
    pg = yield from container.engines['pg']

    with (yield from pg.cursor()) as cur:
            yield from cur.execute('SELECT id AS "Id", randomnumber AS "RandomNumber" FROM world WHERE id=%(idx)s LIMIT 1',
                                   {'idx': randint(1, 10000)})
            world = yield from cur.fetchone()
    return world

@asyncio.coroutine
def get_random_records(container, limit):
    pg = yield from container.engines['pg']
    results = []
    with (yield from pg.cursor()) as cur:
        for i in range(limit):
            yield from cur.execute('SELECT id AS "Id", randomnumber AS "RandomNumber" FROM world WHERE id=%(idx)s LIMIT 1',
                                   {'idx': randint(1, 10000)})
            results.append((yield from cur.fetchone()))

    return results

@asyncio.coroutine
def update_random_records(container, limit):
    results = []
    pg = yield from container.engines['pg']
    with (yield from pg.cursor()) as cur:
        for i in range(limit):
            yield from cur.execute('SELECT id AS "Id", randomnumber AS "RandomNumber" FROM world WHERE id=%(idx)s LIMIT 1',
                                   {'idx': randint(1, 10000)})
            world = yield from cur.fetchone()
            world['RandomNumber'] = randint(1, 10000)
            yield from cur.execute('UPDATE world SET randomnumber=%(random_number)s WHERE id=%(idx)s',
                                   {'random_number': world['RandomNumber'], 'idx': world['Id']})
            results.append(world)
    return results

@asyncio.coroutine
def get_fortunes(container):
    pg = yield from container.engines['pg']

    with (yield from pg.cursor()) as cur:
        yield from cur.execute('SELECT * FROM fortune')
        fortunes = yield from cur.fetchall()

    fortunes.append({'id': 0, 'message': 'Additional fortune added at request time.'})

    fortunes.sort(key=itemgetter('message'))

    return fortunes
