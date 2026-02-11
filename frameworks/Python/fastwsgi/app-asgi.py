import os
import sys
import asyncio
import asyncpg
import jinja2
import random
from operator import itemgetter
from urllib.parse import parse_qs
import asyncache
import cachetools

try:
    from ujson import dumps as jsonify
except:
    from json import dumps as jsonify


db_pool = None

PG_POOL_SIZE = 4

class NoResetConnection(asyncpg.Connection):
    __slots__ = ()

    def get_reset_query(self):
        return ""

async def db_setup():
    global db_pool
    db_pool = await asyncpg.create_pool(
        user=os.getenv('PGUSER', 'benchmarkdbuser'),
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        database='hello_world',
        host='tfb-database',
        port=5432,
        min_size=PG_POOL_SIZE,
        max_size=PG_POOL_SIZE,
        connection_class=NoResetConnection,
    )

READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'

JSON_RESPONSE = {
    'type': 'http.response.start',
    'status': 200,
    'headers': [
        [b'Content-Type', b'application/json'],
    ]
}

HTML_RESPONSE = {
    'type': 'http.response.start',
    'status': 200,
    'headers': [
        [b'Content-Type', b'text/html; charset=utf-8'],
    ]
}

PLAINTEXT_RESPONSE = {
    'type': 'http.response.start',
    'status': 200,
    'headers': [
        [b'Content-Type', b'text/plain; charset=utf-8'],
    ]
}


def get_num_queries(scope, name = b'queries'):
    try:
        query_string = scope['query_string']
        query_count = int(parse_qs(query_string)[name][0])
    except (KeyError, IndexError, ValueError):
        return 1
    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


async def json_serialization(scope, receive, send):
    content = jsonify( {'message': 'Hello, world!'} )
    await send(JSON_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': content.encode('utf8'),
        'more_body': False
    })


async def single_database_query(scope, receive, send):
    row_id = random.randint(1, 10000)
    db_conn = await db_pool.acquire()
    try:
        number = await db_conn.fetchval(READ_ROW_SQL, row_id)
        world = {'id': row_id, 'randomNumber': number}
    finally:
        await db_pool.release(db_conn)

    content = jsonify(world)
    await send(JSON_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': content.encode('utf8'),
        'more_body': False
    })


async def multiple_database_queries(scope, receive, send):
    num_queries = get_num_queries(scope)
    row_ids = random.sample(range(1, 10000), num_queries)
    worlds = [ ]

    async with db_pool.acquire() as db_conn:
        rows = await db_conn.fetchmany(READ_ROW_SQL, [ (v, ) for v in row_ids ] )

    worlds = [ { 'id': row_id, 'randomNumber': number[0] } for row_id, number in zip(row_ids, rows) ]
    content = jsonify(worlds)
    await send(JSON_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': content.encode('utf8'),
        'more_body': False
    })


_get_item1 = itemgetter(1)
fortunes_template = None
fn = os.path.join('templates', 'fortune.html')
with open(fn, 'r') as file:
    text = file.read()
    fortunes_template = jinja2.Template(text)


async def fortunes(scope, receive, send):
    db_conn = await db_pool.acquire()
    try:
        fortunes = await db_conn.fetch('SELECT * FROM Fortune')
    finally:
        await db_pool.release(db_conn)

    fortunes.append([0, "Additional fortune added at request time."])
    fortunes.sort(key = _get_item1)
    content = fortunes_template.render(fortunes=fortunes)
    await send(HTML_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': content.encode('utf8'),
        'more_body': False
    })


async def database_updates(scope, receive, send):
    num_queries = get_num_queries(scope)
    ids = sorted(random.sample(range(1, 10000 + 1), num_queries))
    numbers = sorted(random.sample(range(1, 10000), num_queries))
    updates = list(zip(ids, numbers))
    
    worlds = [ {"id": row_id, "randomNumber": number} for row_id, number in updates ]

    async with db_pool.acquire() as db_conn:
        await db_conn.executemany(READ_ROW_SQL, [ (i[0], ) for i in updates ] )
        await db_conn.executemany(WRITE_ROW_SQL, updates)

    content = jsonify(worlds)
    await send(JSON_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': content.encode('utf8'),
        'more_body': False
    })


from asyncache import cached
from cachetools.keys import hashkey

@cached(cache={}, key=lambda stmt, id: hashkey(id))
async def get_cached_world(stmt, id):
    result = await stmt.fetchrow(id)
    return {'id': result[1], 'randomNumber': result[0]}

async def cached_queries(scope, receive, send):
    count = get_num_queries(scope, b'count')
    row_ids = random.sample(range(1, 10000 + 1), count)
    
    db_conn = await db_pool.acquire()
    try:
        statement = await db_conn.prepare(READ_ROW_SQL)
        worlds = [ await get_cached_world(statement, id) for id in row_ids ]
    finally:
        await db_pool.release(db_conn)

    content = jsonify(worlds)
    await send(JSON_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': content.encode('utf8'),
        'more_body': False
    })


async def plaintext(scope, receive, send):
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
    '/json': json_serialization,
    '/db': single_database_query,
    '/queries': multiple_database_queries,
    '/fortunes': fortunes,
    '/updates': database_updates,
    '/cached-queries': cached_queries,
    '/plaintext': plaintext,
}


async def app(scope, receive, send):
    if scope['type'] == 'lifespan':
        global db_pool
        while True:
            message = await receive()
            if message['type'] == 'lifespan.startup':
                await db_setup()
                await send({'type': 'lifespan.startup.complete'})
            elif message['type'] == 'lifespan.shutdown':
                db_pool.close()
                await send({'type': 'lifespan.shutdown.complete'})
                return            
    else:
        path = scope['path']
        app_handler = routes.get(path, handle_404)
        await app_handler(scope, receive, send)

# -----------------------------------------------------------------------------------------------------

if __name__ == "__main__":
    import multiprocessing
    import fastwsgi

    _is_travis = os.environ.get('TRAVIS') == 'true'

    workers = int(multiprocessing.cpu_count())
    if _is_travis:
        workers = 2

    host = '0.0.0.0'
    port = 3000

    def run_app():
        loop = asyncio.get_event_loop()
        loop.run_until_complete(db_setup())
        fastwsgi.server.backlog = 4096
        fastwsgi.run(app, host, port, loglevel=2)

    def create_fork():
        n = os.fork()
        # n greater than 0 means parent process
        if not n > 0:
            run_app()

    # fork limiting the cpu count - 1
    for i in range(1, workers):
        create_fork()

    run_app()  # run app on the main process too :)
