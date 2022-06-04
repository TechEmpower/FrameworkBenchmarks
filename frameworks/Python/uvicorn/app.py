import asyncio
import asyncpg
import jinja2
import os
import ujson
from random import randint, sample
from operator import itemgetter
from urllib.parse import parse_qs


async def setup():
    global pool
    pool = await asyncpg.create_pool(
        user=os.getenv('PGUSER', 'benchmarkdbuser'),
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        database='hello_world',
        host='tfb-database',
        port=5432
    )


READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, 'Additional fortune added at request time.']

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
json_dumps = ujson.dumps
template = None
path = os.path.join('templates', 'fortune.html')
with open(path, 'r') as template_file:
    template_text = template_file.read()
    template = jinja2.Template(template_text)

loop = asyncio.get_event_loop()
loop.run_until_complete(setup())


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


async def json_serialization(scope, receive, send):
    """
    Test type 1: JSON Serialization
    """
    content = json_dumps({'message': 'Hello, world!'}).encode('utf-8')
    await send(JSON_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': content,
        'more_body': False
    })


async def single_database_query(scope, receive, send):
    """
    Test type 2: Single database object
    """
    row_id = randint(1, 10000)
    connection = await pool.acquire()
    try:
        number = await connection.fetchval(READ_ROW_SQL, row_id)
        world = {'id': row_id, 'randomNumber': number}
    finally:
        await pool.release(connection)

    content = json_dumps(world).encode('utf-8')
    await send(JSON_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': content,
        'more_body': False
    })


async def multiple_database_queries(scope, receive, send):
    """
    Test type 3: Multiple database queries
    """
    num_queries = get_num_queries(scope)
    row_ids = sample(range(1, 10000), num_queries)
    worlds = []

    connection = await pool.acquire()
    try:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append({'id': row_id, 'randomNumber': number})
    finally:
        await pool.release(connection)

    content = json_dumps(worlds).encode('utf-8')
    await send(JSON_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': content,
        'more_body': False
    })


async def fortunes(scope, receive, send):
    """
    Test type 4: Fortunes
    """
    connection = await pool.acquire()
    try:
        fortunes = await connection.fetch('SELECT * FROM Fortune')
    finally:
        await pool.release(connection)

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=key)
    content = template.render(fortunes=fortunes).encode('utf-8')
    await send(HTML_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': content,
        'more_body': False
    })


async def database_updates(scope, receive, send):
    """
    Test type 5: Database updates
    """
    num_queries = get_num_queries(scope)
    updates = [(row_id, randint(1, 10000)) for row_id in sample(range(1, 10000), num_queries)]
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]

    connection = await pool.acquire()
    try:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id, _ in updates:
            await statement.fetchval(row_id)
        await connection.executemany(WRITE_ROW_SQL, updates)
    finally:
        await pool.release(connection)

    content = json_dumps(worlds).encode('utf-8')
    await send(JSON_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': content,
        'more_body': False
    })


async def plaintext(scope, receive, send):
    """
    Test type 6: Plaintext
    """
    content = b'Hello, world!'
    await send(PLAINTEXT_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': content,
        'more_body': False
    })


async def handle_404(scope, receive, send):
    content = b'Not found'
    await send(PLAINTEXT_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': content,
        'more_body': False
    })


routes = {
    '/json': json_serialization,
    '/db': single_database_query,
    '/queries': multiple_database_queries,
    '/fortunes': fortunes,
    '/updates': database_updates,
    '/plaintext': plaintext,
}


async def main(scope, receive, send):
    path = scope['path']
    handler = routes.get(path, handle_404)
    await handler(scope, receive, send)
