import asyncio
from typing import Optional, Any

import asyncpg
import os
import jinja2
from asyncpg import Pool
from starlite import Starlite, get, MediaType
from random import randint
from operator import itemgetter

READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, 'Additional fortune added at request time.']

connection_pool: Pool


async def setup_database():
    global connection_pool
    connection_pool = await asyncpg.create_pool(
        user=os.getenv('PGUSER', 'benchmarkdbuser'),
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        database='hello_world',
        host='tfb-database',
        port=5432
    )


def load_fortunes_template():
    path = os.path.join('templates', 'fortune.html')
    with open(path, 'r') as template_file:
        template_text = template_file.read()
        return jinja2.Template(template_text)


def get_num_queries(queries: Any):
    if queries:
        try:
            query_count = int(queries)
        except (ValueError, TypeError):
            return 1
        if query_count < 1:
            return 1
        if query_count > 500:
            return 500
        return query_count
    return 1


sort_fortunes_key = itemgetter(1)
template = load_fortunes_template()


@get(path='/json')
async def json_serialization() -> dict[str, str]:
    return {'message': 'Hello, world!'}


@get(path='/db')
async def single_database_query() -> dict[str, int]:
    row_id = randint(1, 10000)

    async with connection_pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)

    return {'id': row_id, 'randomNumber': number}


@get(path='/queries')
async def multiple_database_queries(queries: Any = None) -> list[dict[str, int]]:
    num_queries = get_num_queries(queries)
    row_ids = [randint(1, 10000) for _ in range(num_queries)]
    worlds = []

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append({'id': row_id, 'randomNumber': number})

    return worlds


@get(path='/fortunes', media_type=MediaType.HTML)
async def render_fortunes_template() -> str:
    async with connection_pool.acquire() as connection:
        fortunes = await connection.fetch('SELECT * FROM Fortune')

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=sort_fortunes_key)
    return template.render(fortunes=fortunes)


@get(path='/updates')
async def database_updates(queries: Any = None) -> list[dict[str, int]]:
    num_queries = get_num_queries(queries)
    updates = [(randint(1, 10000), randint(1, 10000)) for _ in range(num_queries)]
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id, number in updates:
            await statement.fetchval(row_id)
        await connection.executemany(WRITE_ROW_SQL, updates)

    return worlds


@get(path='/plaintext', media_type=MediaType.TEXT)
async def plaintext() -> bytes:
    return b'Hello, world!'


app = Starlite(
    route_handlers=[
        json_serialization,
        single_database_query,
        multiple_database_queries,
        render_fortunes_template,
        database_updates,
        plaintext
    ],
    on_startup=[setup_database]
)
