import asyncio
import asyncpg
import jinja2
import os
import ujson
from random import randint
from operator import itemgetter
from urllib.parse import parse_qs

from routerling import Router


router = Router()


async def setup():
    global pool
    pool = await asyncpg.create_pool(
        user=os.getenv('PGUSER', 'benchmarkdbuser'),
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        database='hello_world',
        host='tfb-database',
        port=5432
    )


CONTENT_TYPE = 'Content-Type'
JSON = 'application/json'

READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, 'Additional fortune added at request time.']


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


async def json_serialization(r, w, c):
    """
    Test type 1: JSON Serialization
    """
    content = json_dumps({'message': 'Hello, world!'})
    w.headers = CONTENT_TYPE, JSON
    w.body = content


async def single_database_query(r, w, c):
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
    w.headers = CONTENT_TYPE, JSON
    w.body = content


async def multiple_database_queries(r, w, c):
    """
    Test type 3: Multiple database queries
    """
    num_queries = get_num_queries(r._scope)
    row_ids = [randint(1, 10000) for _ in range(num_queries)]
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
    w.headers = CONTENT_TYPE, JSON
    w.body = content


async def fortunes(r, w, c):
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
    w.headers = CONTENT_TYPE, 'text/html; charset=utf-8'
    w.body = content


async def database_updates(r, w, c):
    """
    Test type 5: Database updates
    """
    num_queries = get_num_queries(r._scope)
    updates = [(randint(1, 10000), randint(1, 10000)) for _ in range(num_queries)]
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
    w.headers = CONTENT_TYPE, JSON
    w.body = content


async def plaintext(r, w, c):
    """
    Test type 6: Plaintext
    """
    content = 'Hello, world!'
    w.headers = CONTENT_TYPE, 'text/plain; charset=utf-8'
    w.body = content


async def handle_404(r, w, c):
    content = b'Not found'
    w.headers = CONTENT_TYPE, b'text/plain; charset=utf-8'
    w.body = content



router.HTTP('/json', json_serialization)
router.HTTP('/db', single_database_query)
router.HTTP('/queries', multiple_database_queries)
router.HTTP('/fortunes', fortunes)
router.HTTP('/updates', database_updates)
router.HTTP('/plaintext', plaintext)
