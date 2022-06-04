import asyncio
import asyncpg
import os
import jinja2
import orjson
from logging import getLogger
from random import randint
from operator import itemgetter
from apidaora.asgi.app import asgi_app
from apidaora.asgi.responses import (
    JSON_RESPONSE, HTML_RESPONSE, PLAINTEXT_RESPONSE
)
from apidaora.asgi.router import Route, make_router
from apidaora.method import MethodType


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
        query_count = int(queries[0])
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


def json_serialization(request):
    return JSON_RESPONSE, orjson.dumps({'message': 'Hello, world!'})


async def single_database_query(request):
    row_id = randint(1, 10000)

    async with connection_pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)

    return JSON_RESPONSE, orjson.dumps(
        {'id': row_id, 'randomNumber': number}
    )


async def multiple_database_queries(request):
    num_queries = get_num_queries(request.query_dict.get('queries', 1))
    row_ids = [randint(1, 10000) for _ in range(num_queries)]
    worlds = []

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append(
                dict(
                    id=row_id,
                    randomNumber=number
                )
            )

    return JSON_RESPONSE, orjson.dumps(worlds)


async def fortunes(request):
    async with connection_pool.acquire() as connection:
        fortunes = await connection.fetch('SELECT * FROM Fortune')

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=sort_fortunes_key)
    content = template.render(fortunes=fortunes).encode('utf-8')
    return HTML_RESPONSE, content


async def database_updates(request):
    worlds = []
    updates = set()
    queries = request.query_dict.get('queries', 1)

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL_TO_UPDATE)

        for _ in range(get_num_queries(queries)):
            record = await statement.fetchrow(randint(1, 10000))
            world = dict(
                id=record['id'], randomNumber=record['randomnumber']
            )
            world['randomNumber'] = randint(1, 10000)
            worlds.append(world)
            updates.add((world['id'], world['randomNumber']))

        await connection.executemany(WRITE_ROW_SQL, updates)

    return JSON_RESPONSE, orjson.dumps(worlds)


def plaintext(request):
    return PLAINTEXT_RESPONSE, b'Hello, world!'


routes = (
    Route('/json', MethodType.GET, json_serialization),
    Route('/db', MethodType.GET, single_database_query),
    Route('/queries', MethodType.GET, multiple_database_queries, has_query=True),
    Route('/fortunes', MethodType.GET, fortunes),
    Route('/updates', MethodType.GET, database_updates, has_query=True),
    Route('/plaintext', MethodType.GET, plaintext),
)
router = make_router(routes)
app = asgi_app(router)
