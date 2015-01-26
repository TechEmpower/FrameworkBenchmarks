import asyncio
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
    tasks = []
    results = []
    for i in range(limit):
        tasks.append(container.loop.create_task(get_random_record(container)))
    yield from asyncio.wait(tasks)
    for task in tasks:
        results.append(task.result())
    return results

@asyncio.coroutine
def update_random_record(container):
    pg = yield from container.engines['pg']

    world = yield from get_random_record(container)

    with (yield from pg.cursor()) as cur:
        yield from cur.execute('UPDATE world SET randomnumber=%(random_number)s WHERE id=%(idx)s',
                               {'random_number': randint(1, 10000), 'idx': world['Id']})
    return world

@asyncio.coroutine
def update_random_records(container, limit):
    tasks = []
    results = []
    for i in range(limit):
        tasks.append(container.loop.create_task(update_random_record(container)))
    yield from asyncio.wait(tasks)
    for task in tasks:
        results.append(task.result())
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