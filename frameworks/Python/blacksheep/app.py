import multiprocessing
import os
import asyncpg
import random
import asyncio
from operator import itemgetter
import blacksheep as bs
import jinja2
import msgspec
from pathlib import Path

READ_ROW_SQL = 'SELECT "id", "randomnumber" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, "Additional fortune added at request time."]
MAX_POOL_SIZE = 1000 // multiprocessing.cpu_count()
MIN_POOL_SIZE = max(int(MAX_POOL_SIZE / 2), 1)
db_pool = None
key = itemgetter(1)

try:
    import uvloop
    asyncio.set_event_loop_policy(uvloop.EventLoopPolicy())
except Exception:
    ...

async def setup_db(app):
    global db_pool
    db_pool = await asyncpg.create_pool(
        user=os.getenv('PGUSER', "benchmarkdbuser"),
        password=os.getenv('PGPASS', "benchmarkdbpass"),
        database='hello_world',
        host="tfb-database",
        port=5432,
        min_size=MIN_POOL_SIZE,
        max_size=MAX_POOL_SIZE,
    )


def load_fortunes_template():
    with Path("templates/fortune.html").open("r") as f:
        return jinja2.Template(f.read())


fortune_template = load_fortunes_template()

app = bs.Application()
app.on_start += setup_db


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

ENCODER = msgspec.json.Encoder()
DECODER = msgspec.json.Decoder()
JSON_CONTENT_TYPE = b"application/json"
def jsonify(
    data,
    status=200,
    headers=None,
):
    """
    Returns a response with application/json content,
    and given status (default HTTP 200 OK).
    """
    return bs.Response(
        status=status,
        headers=headers,
        content=bs.Content(content_type=JSON_CONTENT_TYPE, data=ENCODER.encode(data)),
    )


# ------------------------------------------------------------------------------------------

@bs.get('/json')
async def json_test(request):
    return jsonify( {'message': 'Hello, world!'} )

@bs.get('/db')
async def single_db_query_test(request):
    row_id = random.randint(1, 10000)
    
    async with db_pool.acquire() as db_conn:
        number = await db_conn.fetchval(READ_ROW_SQL, row_id)
    
    return jsonify({'id': row_id, 'randomNumber': number})


@bs.get('/queries')
async def multiple_db_queries_test(request):
    num_queries = get_num_queries(request)
    row_ids = random.sample(range(1, 10000), num_queries)
    worlds = []

    async with db_pool.acquire() as db_conn:
        statement = await db_conn.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append( {"id": row_id, "randomNumber": number} )

    return jsonify(worlds)


@bs.get('/fortunes')
async def fortunes_test(request):
    async with db_pool.acquire() as db_conn:
        fortunes = await db_conn.fetch("SELECT * FROM Fortune")

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key = key)
    data = fortune_template.render(fortunes=fortunes)
    return bs.html(data)


@bs.get('/updates')
async def db_updates_test(request):
    num_queries = get_num_queries(request)
    ids = sorted(random.sample(range(1, 10000 + 1), num_queries))
    numbers = sorted(random.sample(range(1, 10000), num_queries))
    updates = list(zip(ids, numbers))

    worlds = [ {"id": row_id, "randomNumber": number} for row_id, number in updates ]

    async with db_pool.acquire() as db_conn:
        statement = await db_conn.prepare(READ_ROW_SQL)
        for row_id, _ in updates:
            await statement.fetchval(row_id)
        await db_conn.executemany(WRITE_ROW_SQL, updates)
 
    return jsonify(worlds)


@bs.get('/plaintext')
async def plaintext_test(request):
    return bs.Response(200, content=bs.Content(b"text/plain", b'Hello, World!'))
    #return bs.text('Hello, World!')

