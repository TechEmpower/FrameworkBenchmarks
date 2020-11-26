import asyncio
import asyncpg
import os
import jinja2
from logging import getLogger
from apidaora import appdaora, html, route, text
from random import randint
from operator import itemgetter
from typing import TypedDict, Optional


logger = getLogger(__name__)


READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
READ_ROW_SQL_TO_UPDATE = 'SELECT "id", "randomnumber" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, 'Additional fortune added at request time.']


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


def get_num_queries(queries):
    try:
        query_count = int(queries)
    except (ValueError, TypeError):
        return 1

    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


connection_pool = None
sort_fortunes_key = itemgetter(1)
template = load_fortunes_template()
loop = asyncio.get_event_loop()
loop.run_until_complete(setup_database())


@route.get('/json')
async def json_serialization():
    return {'message': 'Hello, world!'}


class DatabaseObject(TypedDict):
    id: int
    randomNumber: float


@route.get('/db')
async def single_database_query():
    row_id = randint(1, 10000)

    async with connection_pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)

    return DatabaseObject(id=row_id, randomNumber=number)


@route.get('/queries')
async def multiple_database_queries(queries: Optional[str] = None):
    num_queries = get_num_queries(queries)
    row_ids = [randint(1, 10000) for _ in range(num_queries)]
    worlds = []

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append(
                DatabaseObject(
                    id=row_id,
                    randomNumber=number
                )
            )

    return worlds


@route.get('/fortunes')
async def fortunes():
    async with connection_pool.acquire() as connection:
        fortunes = await connection.fetch('SELECT * FROM Fortune')

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=sort_fortunes_key)
    content = template.render(fortunes=fortunes)
    return html(content)


@route.get('/updates')
async def database_updates(queries: Optional[str] = None):
    worlds = []
    updates = set()

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL_TO_UPDATE)

        for _ in range(get_num_queries(queries)):
            record = await statement.fetchrow(randint(1, 10000))
            world = DatabaseObject(
                id=record['id'], randomNumber=record['randomnumber']
            )
            world['randomNumber'] = randint(1, 10000)
            worlds.append(world)
            updates.add((world['id'], world['randomNumber']))

        await connection.executemany(WRITE_ROW_SQL, updates)

    return worlds


@route.get('/plaintext')
async def plaintext():
    return text('Hello, world!')


app = appdaora([
    json_serialization,
    single_database_query,
    multiple_database_queries,
    fortunes,
    database_updates,
    plaintext
])
