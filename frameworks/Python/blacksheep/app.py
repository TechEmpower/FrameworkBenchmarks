import os
import ujson
import asyncpg
from random import randint
from multiprocessing import cpu_count
from blacksheep.server import Application
from blacksheep import Response, Header, Content
from jinja2 import Template
json_dumps = ujson.dumps


async def configure_db(app):
    global db_pool
    db_pool = await asyncpg.create_pool(
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
        return Template(template_text)


db_pool = None
fortune_template = load_fortunes_template()

app = Application()
app.on_start += configure_db


def get_num_queries(request):
    try:
        value = request.query.get('queries')
        if value is None:
            return 1

        query_count = int(value[0])
    except (KeyError, IndexError, ValueError):
        return 1

    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


@app.route('/json')
async def json_test(request):
    """Test type 1: JSON Serialization"""

    return Response(200, content=Content(b'application/json; charset=utf-8',
                                         json_dumps({'message': 'Hello, world!'}).encode('utf-8')))


@app.route('/db')
async def single_db_query_test(request):
    """Test type 2: Single Database Query"""

    row_id = randint(1, 10000)
    connection = await db_pool.acquire()
    try:
        number = await connection.fetchval('SELECT "randomnumber", "id" FROM "world" WHERE id = $1', row_id)
        world = {'id': row_id, 'randomNumber': number}
    finally:
        await db_pool.release(connection)

    return Response(200, content=Content(b'application/json; charset=utf-8',
                                         json_dumps(world).encode('utf-8')))


@app.route('/queries')
async def multiple_db_queries_test(request):
    """Test type 3: Multiple Database Queries"""

    num_queries = get_num_queries(request)

    row_ids = [randint(1, 10000) for _ in range(num_queries)]
    worlds = []

    connection = await db_pool.acquire()
    try:
        statement = await connection.prepare('SELECT "randomnumber", "id" FROM "world" WHERE id = $1')
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append({'id': row_id, 'randomNumber': number})
    finally:
        await db_pool.release(connection)

    return Response(200, content=Content(b'application/json; charset=utf-8',
                                         json_dumps(worlds).encode('utf-8')))


@app.route('/fortunes')
async def fortunes_test(request):
    """Test type 4: Fortunes"""

    connection = await db_pool.acquire()

    try:
        fortunes = await connection.fetch('SELECT * FROM Fortune')
    finally:
        await db_pool.release(connection)

    fortunes.append([0, 'Additional fortune added at request time.'])
    fortunes.sort(key=lambda x: x[1])

    return Response(200, [
        Header(b'Cache-Control', b'no-cache')
    ], content=Content(b'text/html; charset=utf-8', fortune_template.render(fortunes=fortunes).encode('utf8')))


@app.route('/updates')
async def db_updates_test(request):
    """Test type 5: Database Updates"""

    num_queries = get_num_queries(request)

    updates = [(randint(1, 10000), randint(1, 10000)) for _ in range(num_queries)]
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]

    connection = await db_pool.acquire()
    try:
        statement = await connection.prepare('SELECT "randomnumber", "id" FROM "world" WHERE id = $1')
        for row_id, _ in updates:
            await statement.fetchval(row_id)
        await connection.executemany('UPDATE "world" SET "randomnumber"=$1 WHERE id=$2', updates)
    finally:
        await db_pool.release(connection)

    return Response(200, content=Content(b'application/json',
                                         json_dumps(worlds).encode('utf-8')))


@app.route('/plaintext')
async def plaintext_test(request):
    """Test type 6: Plaintext"""

    return Response(200, content=Content(b'text/plain', b'Hello, World!'))
