import multiprocessing
from random import randint, sample

import asyncpg
import jinja2
from panther import Panther
from panther.app import API
from panther.events import Event
from panther.request import Request
from panther.response import Response, PlainTextResponse, HTMLResponse

cpu_count = multiprocessing.cpu_count()
MAX_POOL_SIZE = 1000 // cpu_count
MIN_POOL_SIZE = max(MAX_POOL_SIZE // 2, 1)

connection_pool = None

fortune_template = jinja2.Environment(
    loader=jinja2.FileSystemLoader('templates'),
    autoescape=True
).get_template('fortune.html')


@Event.startup
async def on_startup():
    global connection_pool
    connection_pool = await asyncpg.create_pool(
        user='benchmarkdbuser',
        password='benchmarkdbpass',
        database='hello_world',
        host='tfb-database',
        port=5432,
        min_size=MIN_POOL_SIZE,
        max_size=MAX_POOL_SIZE,
    )


@Event.shutdown
async def on_shutdown():
    await connection_pool.close()


@API()
def json_serialization():
    return Response(data={'message': 'Hello, world!'})


@API()
async def single_database_query():
    row_id = randint(1, 10000)
    async with connection_pool.acquire() as connection:
        number = await connection.fetchval('SELECT id, randomnumber FROM world WHERE id = $1', row_id)
    return Response(data={'id': row_id, 'randomNumber': number})


@API()
async def multiple_database_queries(request: Request):
    try:
        count = int(request.query_params.get('queries', 1))
    except (ValueError, TypeError):
        count = 1
    row_ids = sample(range(1, 10000), min(max(count, 1), 500))
    async with connection_pool.acquire() as connection:
        statement = await connection.prepare('SELECT id, randomnumber FROM world WHERE id = $1')
        worlds = [{'id': i, 'randomNumber': await statement.fetchval(i)} for i in row_ids]
    return Response(data=worlds)


@API()
async def fortunes():
    async with connection_pool.acquire() as connection:
        fortune_records = await connection.fetch('SELECT * FROM Fortune')
    fortune_records = [(row['id'], row['message']) for row in fortune_records]
    fortune_records.append((0, 'Additional fortune added at request time.'))
    fortune_records.sort(key=lambda row: row[1])
    data = fortune_template.render(fortunes=fortune_records)
    return HTMLResponse(data=data)


@API()
async def database_updates(request: Request):
    try:
        count = int(request.query_params.get('queries', 1))
    except (ValueError, TypeError):
        count = 1
    num_queries = min(max(count, 1), 500)

    updates = list(zip(
        sample(range(1, 10000), num_queries),
        sorted(sample(range(1, 10000), num_queries))
    ))
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]
    async with connection_pool.acquire() as connection:
        statement = await connection.prepare('SELECT id, randomnumber FROM world WHERE id = $1')
        for _, row_id in updates:
            await statement.fetchval(row_id)
        await connection.executemany('UPDATE world SET randomnumber = $1 WHERE id = $2', updates)
    return Response(data=worlds)


@API()
def plaintext():
    return PlainTextResponse(b'Hello, world!')


url_routing = {
    'json': json_serialization,
    'db': single_database_query,
    'queries': multiple_database_queries,
    'fortunes': fortunes,
    'updates': database_updates,
    'plaintext': plaintext,
}

app = Panther(__name__, configs=__name__, urls=url_routing)
