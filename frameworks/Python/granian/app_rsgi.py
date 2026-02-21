import os

from operator import itemgetter
from pathlib import Path
from random import randint, sample
from urllib.parse import parse_qs

import asyncpg
import jinja2
import orjson

PG_POOL_SIZE = 4


class NoResetConnection(asyncpg.Connection):
    __slots__ = ()

    def get_reset_query(self):
        return ""


async def pg_setup():
    global pool
    pool = await asyncpg.create_pool(
        user=os.getenv('PGUSER', 'benchmarkdbuser'),
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        database='hello_world',
        host='tfb-database',
        port=5432,
        min_size=PG_POOL_SIZE,
        max_size=PG_POOL_SIZE,
        connection_class=NoResetConnection,
    )


SQL_SELECT = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
SQL_UPDATE = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'

JSON_HEADERS = [('content-type', 'application/json')]
HTML_HEADERS = [('content-type', 'text/html; charset=utf-8')]
PLAINTEXT_HEADERS = [('content-type', 'text/plain; charset=utf-8')]

pool = None
key = itemgetter(1)
json_dumps = orjson.dumps

with Path('templates/fortune.html').open('r') as f:
    template = jinja2.Template(f.read())


def get_num_queries(scope):
    try:
        query_count = int(parse_qs(scope.query_string)['queries'][0])
    except (KeyError, IndexError, ValueError):
        return 1

    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


async def route_json(scope, proto):
    proto.response_bytes(
        200,
        JSON_HEADERS,
        json_dumps({'message': 'Hello, world!'})
    )


async def route_db(scope, proto):
    row_id = randint(1, 10000)
    async with pool.acquire() as connection:
        number = await connection.fetchval(SQL_SELECT, row_id)

    proto.response_bytes(
        200,
        JSON_HEADERS,
        json_dumps({'id': row_id, 'randomNumber': number})
    )


async def route_queries(scope, proto):
    num_queries = get_num_queries(scope)
    row_ids = sample(range(1, 10000), num_queries)
    worlds = []

    async with pool.acquire() as connection:
        rows = await connection.fetchmany(SQL_SELECT, [(v,) for v in row_ids])

    worlds = [{'id': row_id, 'randomNumber': number[0]} for row_id, number in zip(row_ids, rows)]
    proto.response_bytes(
        200,
        JSON_HEADERS,
        json_dumps(worlds)
    )


async def route_fortunes(scope, proto):
    async with pool.acquire() as connection:
        fortunes = await connection.fetch('SELECT * FROM Fortune')

    fortunes.append([0, 'Additional fortune added at request time.'])
    fortunes.sort(key=key)
    content = template.render(fortunes=fortunes)
    proto.response_str(
        200,
        HTML_HEADERS,
        content
    )


async def route_updates(scope, proto):
    num_queries = get_num_queries(scope)
    updates = list(zip(
        sample(range(1, 10000), num_queries),
        sorted(sample(range(1, 10000), num_queries))
    ))
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]

    async with pool.acquire() as connection:
        await connection.executemany(SQL_SELECT, [(i[0],) for i in updates])
        await connection.executemany(SQL_UPDATE, updates)

    proto.response_bytes(
        200,
        JSON_HEADERS,
        json_dumps(worlds)
    )


async def route_plaintext(scope, proto):
    proto.response_bytes(
        200,
        PLAINTEXT_HEADERS,
        b'Hello, world!'
    )


async def handle_404(scope, proto):
    proto.response_bytes(
        404,
        PLAINTEXT_HEADERS,
        b'Not found'
    )


routes = {
    '/json': route_json,
    '/db': route_db,
    '/queries': route_queries,
    '/fortunes': route_fortunes,
    '/updates': route_updates,
    '/plaintext': route_plaintext
}


class App:
    def __rsgi_init__(self, loop):
        loop.run_until_complete(pg_setup())

    def __rsgi__(self, scope, proto):
        handler = routes.get(scope.path, handle_404)
        return handler(scope, proto)


main = App()
