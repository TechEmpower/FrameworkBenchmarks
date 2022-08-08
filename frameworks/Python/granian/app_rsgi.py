import asyncio
import os

from operator import itemgetter
from pathlib import Path
from random import randint
from urllib.parse import parse_qs

import asyncpg
import jinja2
import orjson

from granian.rsgi import Response


async def pg_setup():
    global pool
    pool = await asyncpg.create_pool(
        user=os.getenv('PGUSER', 'benchmarkdbuser'),
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        database='hello_world',
        host='tfb-database',
        port=5432
    )


SQL_SELECT = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
SQL_UPDATE = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ROW_ADD = [0, 'Additional fortune added at request time.']

JSON_HEADERS = {'content-type': 'application/json'}
HTML_HEADERS = {'content-type': 'text/html; charset=utf-8'}
PLAINTEXT_HEADERS = {'content-type': 'text/plain; charset=utf-8'}

pool = None
key = itemgetter(1)
json_dumps = orjson.dumps

with Path('templates/fortune.html').open('r') as f:
    template = jinja2.Template(f.read())

asyncio.get_event_loop().run_until_complete(pg_setup())


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
    return Response.bytes(json_dumps({'message': 'Hello, world!'}), 200, JSON_HEADERS)


async def route_db(scope, proto):
    row_id = randint(1, 10000)
    connection = await pool.acquire()
    try:
        number = await connection.fetchval(SQL_SELECT, row_id)
        world = {'id': row_id, 'randomNumber': number}
    finally:
        await pool.release(connection)

    return Response.bytes(json_dumps(world), 200, JSON_HEADERS)


async def route_queries(scope, proto):
    num_queries = get_num_queries(scope)
    row_ids = [randint(1, 10000) for _ in range(num_queries)]
    worlds = []

    connection = await pool.acquire()
    try:
        statement = await connection.prepare(SQL_SELECT)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append({'id': row_id, 'randomNumber': number})
    finally:
        await pool.release(connection)

    return Response.bytes(json_dumps(worlds), 200, JSON_HEADERS)


async def route_fortunes(scope, proto):
    connection = await pool.acquire()
    try:
        fortunes = await connection.fetch('SELECT * FROM Fortune')
    finally:
        await pool.release(connection)

    fortunes.append(ROW_ADD)
    fortunes.sort(key=key)
    content = template.render(fortunes=fortunes)
    return Response.str(content, 200, HTML_HEADERS)


async def route_updates(scope, proto):
    num_queries = get_num_queries(scope)
    updates = [(randint(1, 10000), randint(1, 10000)) for _ in range(num_queries)]
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]

    connection = await pool.acquire()
    try:
        statement = await connection.prepare(SQL_SELECT)
        for row_id, _ in updates:
            await statement.fetchval(row_id)
        await connection.executemany(SQL_UPDATE, updates)
    finally:
        await pool.release(connection)

    return Response.bytes(json_dumps(worlds), 200, JSON_HEADERS)


async def route_plaintext(scope, proto):
    return Response.bytes(b'Hello, world!', 200, PLAINTEXT_HEADERS)


async def handle_404(scope, proto):
    return Response.bytes(b'Not found', 404, PLAINTEXT_HEADERS)


routes = {
    '/json': route_json,
    '/db': route_db,
    '/queries': route_queries,
    '/fortunes': route_fortunes,
    '/updates': route_updates,
    '/plaintext': route_plaintext
}


def main(scope, proto):
    handler = routes.get(scope.path, handle_404)
    return handler(scope, proto)
