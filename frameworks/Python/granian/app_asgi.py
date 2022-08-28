import asyncio
import os

from operator import itemgetter
from pathlib import Path
from random import randint
from urllib.parse import parse_qs

import asyncpg
import jinja2
import orjson


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

JSON_RESPONSE = {
    'type': 'http.response.start',
    'status': 200,
    'headers': [
        [b'content-type', b'application/json'],
    ]
}
HTML_RESPONSE = {
    'type': 'http.response.start',
    'status': 200,
    'headers': [
        [b'content-type', b'text/html; charset=utf-8'],
    ]
}
PLAINTEXT_RESPONSE = {
    'type': 'http.response.start',
    'status': 200,
    'headers': [
        [b'content-type', b'text/plain; charset=utf-8'],
    ]
}

pool = None
key = itemgetter(1)
json_dumps = orjson.dumps

with Path('templates/fortune.html').open('r') as f:
    template = jinja2.Template(f.read())

asyncio.get_event_loop().run_until_complete(pg_setup())


def get_num_queries(scope):
    try:
        query_string = scope['query_string']
        query_count = int(parse_qs(query_string)[b'queries'][0])
    except (KeyError, IndexError, ValueError):
        return 1

    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


async def route_json(scope, receive, send):
    await send(JSON_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': json_dumps({'message': 'Hello, world!'}),
        'more_body': False
    })


async def route_db(scope, receive, send):
    row_id = randint(1, 10000)
    connection = await pool.acquire()
    try:
        number = await connection.fetchval(SQL_SELECT, row_id)
        world = {'id': row_id, 'randomNumber': number}
    finally:
        await pool.release(connection)

    await send(JSON_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': json_dumps(world),
        'more_body': False
    })


async def route_queries(scope, receive, send):
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

    await send(JSON_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': json_dumps(worlds),
        'more_body': False
    })


async def route_fortunes(scope, receive, send):
    connection = await pool.acquire()
    try:
        fortunes = await connection.fetch('SELECT * FROM Fortune')
    finally:
        await pool.release(connection)

    fortunes.append(ROW_ADD)
    fortunes.sort(key=key)
    content = template.render(fortunes=fortunes).encode('utf-8')
    await send(HTML_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': content,
        'more_body': False
    })


async def route_updates(scope, receive, send):
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

    await send(JSON_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': json_dumps(worlds),
        'more_body': False
    })


async def route_plaintext(scope, receive, send):
    await send(PLAINTEXT_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': b'Hello, world!',
        'more_body': False
    })


async def handle_404(scope, receive, send):
    await send(PLAINTEXT_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': b'Not found',
        'more_body': False
    })


routes = {
    '/json': route_json,
    '/db': route_db,
    '/queries': route_queries,
    '/fortunes': route_fortunes,
    '/updates': route_updates,
    '/plaintext': route_plaintext
}


def main(scope, receive, send):
    handler = routes.get(scope['path'], handle_404)
    return handler(scope, receive, send)
