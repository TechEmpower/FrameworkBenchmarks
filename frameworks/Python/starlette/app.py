import asyncio
import asyncpg
import os
import jinja2
from starlette import (
    asgi_application, HTMLResponse, JSONResponse, Path, PlainTextResponse, Router
)
from random import randint
from operator import itemgetter
from urllib.parse import parse_qs
from ujson import dumps as ujson_dumps


READ_ROW_SQL = 'SELECT "randomnumber" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, 'Additional fortune added at request time.']


class UJSONResponse(JSONResponse):
    def render(self, content):
        return ujson_dumps(content).encode('utf-8')


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


def get_num_queries(request):
    try:
        query_string = request['query_string']
        query_count = int(parse_qs(query_string)[b'queries'][0])
    except (KeyError, IndexError, ValueError):
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


@asgi_application
def json_serialization(request):
    return UJSONResponse({'message': 'Hello, world!'})


@asgi_application
async def single_database_query(request):
    row_id = randint(1, 10000)

    async with connection_pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)

    return UJSONResponse({'id': row_id, 'randomNumber': number})


@asgi_application
async def multiple_database_queries(request):
    num_queries = get_num_queries(request)
    row_ids = [randint(1, 10000) for _ in range(num_queries)]
    worlds = []

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append({'id': row_id, 'randomNumber': number})

    return UJSONResponse(worlds)


@asgi_application
async def fortunes(request):
    async with connection_pool.acquire() as connection:
        fortunes = await connection.fetch('SELECT * FROM Fortune')

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=sort_fortunes_key)
    content = template.render(fortunes=fortunes)
    return HTMLResponse(content)


@asgi_application
async def database_updates(request):
    num_queries = get_num_queries(request)
    updates = [(randint(1, 10000), randint(1, 10000)) for _ in range(num_queries)]
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id, number in updates:
            await statement.fetchval(row_id)
        await connection.executemany(WRITE_ROW_SQL, updates)

    return UJSONResponse(worlds)


@asgi_application
def plaintext(request):
    return PlainTextResponse(b'Hello, world!')


app = Router(routes=[
    Path('/json', json_serialization),
    Path('/db', single_database_query),
    Path('/queries', multiple_database_queries),
    Path('/fortunes', fortunes),
    Path('/updates', database_updates),
    Path('/plaintext', plaintext),
])
