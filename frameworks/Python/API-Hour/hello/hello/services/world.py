import asyncio
from pprint import pprint
from random import randint

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