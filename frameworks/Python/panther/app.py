import multiprocessing
from pathlib import Path
from random import randint, sample

import asyncpg
import jinja2
from panther import Panther
from panther.app import API
from panther.events import Event
from panther.request import Request
from panther.response import Response, PlainTextResponse, HTMLResponse

READ_ROW_SQL = 'SELECT "id", "randomnumber" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, 'Additional fortune added at request time.']
MAX_POOL_SIZE = 1000 // multiprocessing.cpu_count()
MIN_POOL_SIZE = max(int(MAX_POOL_SIZE / 2), 1)

pool = None


@Event.startup
async def create_db_pool():
    global pool
    pool = await asyncpg.create_pool(
        user='benchmarkdbuser',
        password='benchmarkdbpass',
        database='hello_world',
        host='tfb-database',
        port=5432,
        min_size=MIN_POOL_SIZE,
        max_size=MAX_POOL_SIZE,
    )


@Event.shutdown
async def clean_db_pool():
    await pool.close()


with Path('templates/fortune.html').open() as f:
    fortune_template = jinja2.Template(f.read())


def get_num_queries(request):
    value = request.query_params.get('queries')
    if value is None:
        return 1

    try:
        query_count = int(value)
    except ValueError:
        return 1
    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


@API()
async def json_serialization():
    return Response(data={'message': 'Hello, world!'})


@API()
async def single_database_query():
    row_id = randint(1, 10000)
    async with pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)
    return Response(data={'id': row_id, 'randomNumber': number})


@API()
async def multiple_database_queries(request: Request):
    num_queries = get_num_queries(request)
    row_ids = sample(range(1, 10000), num_queries)

    async with pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        worlds = [{'id': i, 'randomNumber': await statement.fetchval(i)} for i in row_ids]

    return Response(data=worlds)


@API()
async def fortunes():
    async with pool.acquire() as connection:
        fortune_records = await connection.fetch('SELECT * FROM Fortune')
    fortune_records.append(ADDITIONAL_ROW)
    fortune_records.sort(key=lambda row: row[1])
    data = fortune_template.render(fortunes=fortune_records)
    return HTMLResponse(data=data)


@API()
async def database_updates(request: Request):
    num_queries = get_num_queries(request)
    updates = list(zip(
        sample(range(1, 10000), num_queries),
        sorted(sample(range(1, 10000), num_queries))
    ))

    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]

    async with pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for _, row_id in updates:
            await statement.fetchval(row_id)
        await connection.executemany(WRITE_ROW_SQL, updates)
    return Response(data=worlds)


@API()
async def plaintext():
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
