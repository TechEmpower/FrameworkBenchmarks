import asyncio
import os
from operator import itemgetter
from random import randint, sample
from typing import Any

import uvloop
from asyncpg import create_pool
from jinja2 import Template
from starlite import MediaType, Starlite, get

asyncio.set_event_loop_policy(uvloop.EventLoopPolicy())
connection_pool: Any = None


async def init_connection_pool() -> None:
    global connection_pool
    connection_pool = await create_pool(
        user=os.getenv('PGUSER', 'benchmarkdbuser'),
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        database='hello_world',
        host='tfb-database',
        port=5432
    )


def normalize_queries(value: str | None) -> int:
    queries = int(value) if value and value.isnumeric() else 1
    if queries > 500:
        return 500
    if queries < 1:
        return 1
    return queries


def load_fortunes_template() -> "Template":
    path = os.path.join('templates', 'fortune.html')
    with open(path, 'r') as template_file:
        template_text = template_file.read()
        return Template(template_text)


fortune_template = load_fortunes_template()


@get(path='/json')
def json_serialization() -> dict[str, str]:
    return {'message': 'Hello, world!'}


@get(path='/db')
async def single_database_query() -> dict[str, int]:
    row_id = randint(1, 10000)
    async with connection_pool.acquire() as connection:
        number = await connection.fetchval(
            'SELECT "randomnumber", "id" FROM "world" WHERE id = $1',
            row_id
        )

    return {'id': row_id, 'randomNumber': number}


@get(path='/queries')
async def multiple_database_queries(queries: None | str = None) -> list[dict[str, int]]:
    row_ids = sample(range(1, 10000), normalize_queries(queries))
    worlds = []

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare('SELECT "randomnumber", "id" FROM "world" WHERE id = $1')
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append({'id': row_id, 'randomNumber': number})

    return worlds


@get(path='/fortunes', media_type=MediaType.HTML)
async def render_fortunes_template() -> str:
    async with connection_pool.acquire() as connection:
        fortunes = await connection.fetch('SELECT * FROM Fortune')

    fortunes.append([0, 'Additional fortune added at request time.'])
    fortunes.sort(key=itemgetter(1))
    return fortune_template.render(fortunes=fortunes)


@get(path='/updates')
async def database_updates(queries: None | str = None) -> list[dict[str, int]]:
    num_queries = normalize_queries(queries)
    updates = list(zip(sorted(sample(range(1, 10000 + 1), num_queries)), sample(range(1, 10000), num_queries)))

    worlds = [
        {"id": row_id, "randomNumber": number} for row_id, number in updates
    ]

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare('SELECT "id", "randomnumber" FROM "world" WHERE id = $1')
        for row_id, _ in updates:
            await statement.fetchval(row_id)
        await connection.executemany('UPDATE "world" SET "randomnumber"=$1 WHERE id=$2', updates)

    return worlds


@get(path='/plaintext', media_type=MediaType.TEXT)
def plaintext() -> bytes:
    return b'Hello, world!'


app = Starlite(
    route_handlers=[
        database_updates,
        json_serialization,
        multiple_database_queries,
        plaintext,
        render_fortunes_template,
        single_database_query,
    ],
    on_startup=[init_connection_pool],
    openapi_config=None,
)
