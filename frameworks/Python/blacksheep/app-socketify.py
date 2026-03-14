import multiprocessing
import os
# import psycopg
import platform
import random
import asyncio
import blacksheep as bs
import jinja2
from pathlib import Path
# from psycopg_pool import AsyncConnectionPool
import psqlpy
from psqlpy import ConnectionPoolBuilder
READ_ROW_SQL = 'SELECT "randomnumber" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
CORE_COUNT = multiprocessing.cpu_count()
MAX_DB_CONNECTIONS = 2000

MAX_POOL_SIZE = min(CORE_COUNT * 2, MAX_DB_CONNECTIONS // CORE_COUNT, 35)
db_pool = None

async def setup_db(app):
    global db_pool
    conninfo = (
        f"postgresql://{os.getenv('PGUSER', 'benchmarkdbuser')}:{os.getenv('PGPASS', 'benchmarkdbpass')}"
        f"@tfb-database:5432/hello_world"
    )
    db_pool = (
        ConnectionPoolBuilder()
        .max_pool_size(MAX_POOL_SIZE)
        .user(os.getenv('PGUSER', 'benchmarkdbuser'))
        .password(os.getenv('PGPASS', 'benchmarkdbpass'))
        .dbname("hello_world")
        .host("tfb-database")
        .port(5432)
        .build()
    )
    # await db_pool.open()

async def shutdown_db(app):
    global db_pool
    if db_pool is not None:
        await db_pool.close()
        db_pool = None

def load_fortunes_template():
    with Path("templates/fortune.html").open("r") as f:
        return jinja2.Template(f.read())

fortune_template = load_fortunes_template()

app = bs.Application()
app.on_start += setup_db
app.on_stop += shutdown_db

def get_num_queries(request):
    try:
        value = request.query.get('queries')
        if value is None:
            return 1
        query_count = int(value[0])
    except (KeyError, IndexError, ValueError):
        return 1
    return min(max(query_count, 1), 500)

JSON_CONTENT_TYPE = b"application/json"

@bs.get('/json')
async def json_test(request):
    return bs.json({'message': 'Hello, world!'})

@bs.get('/db')
async def single_db_query_test(request):
    row_id = random.randint(1, 10000)
    async with db_pool.acquire() as connection:
        number = await connection.fetch_val(
            READ_ROW_SQL, [row_id]
        )
    return bs.json({'id': row_id, 'randomNumber': number})

@bs.get('/queries')
async def multiple_db_queries_test(request):
    num_queries = get_num_queries(request)
    row_ids = random.sample(range(1, 10000), num_queries)
    worlds = []
    async with db_pool.acquire() as connection:
        for row_id in row_ids:
            number = await connection.fetch_val(
                READ_ROW_SQL, [row_id]
            )
            worlds.append({"id": row_id, "randomNumber": number})
    return bs.json(worlds)

@bs.get('/fortunes')
async def fortunes_test(request):
    async with db_pool.acquire() as connection:
        fortunes_fetch = await connection.fetch("SELECT * FROM Fortune")
        # fortunes = fortunes_fetch.result()
        fortunes = [list(item.values()) for item in fortunes_fetch.result()]
    fortunes.append([0, "Additional fortune added at request time."])
    fortunes.sort(key=lambda row: row[1])
    data = fortune_template.render(fortunes=fortunes)
    return bs.html(data)

@bs.get('/updates')
async def db_updates_test(request):
    num_queries = get_num_queries(request)
    updates = sorted(zip(
        random.sample(range(1, 10000), num_queries),
        random.sample(range(1, 10000), num_queries)
    ), key=lambda x: x[1])
    worlds = [{"id": row_id, "randomNumber": number} for row_id, number in updates]
    async with db_pool.acquire() as connection:
        for row_id, _ in updates:
            await connection.fetch_val(READ_ROW_SQL, [row_id])
        # await db_conn.executemany(WRITE_ROW_SQL, updates)
        await connection.execute_many(WRITE_ROW_SQL, updates)
    return bs.json(worlds)

@bs.get('/plaintext')
async def plaintext_test(request):
    return bs.Response(200, content=bs.Content(b"text/plain", b'Hello, World!'))

if platform.python_implementation() == 'PyPy':
    import logging
    from socketify import ASGI
    workers = int(multiprocessing.cpu_count())
    if os.environ.get('TRAVIS') == 'true':
        workers = 2

    def run_app():
        ASGI(app).listen(8080, lambda config: logging.info(f"Listening on port http://localhost:{config.port} now\n")).run()

    def create_fork():
        n = os.fork()
        if not n > 0:
            run_app()

    for i in range(1, workers):
        create_fork()
    run_app()
