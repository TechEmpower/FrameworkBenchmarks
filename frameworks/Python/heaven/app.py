from multiprocessing import cpu_count, pool
from os import getenv
from random import randint, sample

from asyncpg import create_pool
from heaven import Application, Context, Request, Response
from heaven.constants import STARTUP, SHUTDOWN
from orjson import dumps


#############
# constants #
#############
APPLICATION_JSON = 'application/json'
CONTENT_TYPE = 'Content-Type'
POOL = 'pool'
READ_ROW_SQL = 'SELECT "id", "randomnumber" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
MAX_POOL_SIZE = 1000//cpu_count()
MIN_POOL_SIZE = max(int(MAX_POOL_SIZE / 2), 1)


#########################################
# HOOKS for the app (shutdown, startup) #
#########################################
async def up_database(app: Application):
    pool = await create_pool(
        user=getenv("PGUSER", "benchmarkdbuser"),
        password=getenv("PGPASS", "benchmarkdbpass"),
        database="hello_world",
        host="tfb-database",
        port=5432,
        min_size=MIN_POOL_SIZE,
        max_size=MAX_POOL_SIZE,
    )
    app.keep(POOL, pool)


async def down_database(app: Application):
    pool = app.unkeep(POOL)
    await pool.close()


################
# Helper utils #
################
def get_num_queries(queries):
    try: query_count = int(queries)
    except (ValueError, TypeError): return 1

    if query_count < 1: return 1
    if query_count > 500: return 500
    return query_count


###############################
# Handlers for the app routes #
###############################
async def database(req: Request, res: Response, ctx: Context):
    row_id = randint(1, 10000)
    pool = req.app.peek(POOL)
    async with pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)

    res.headers = CONTENT_TYPE, APPLICATION_JSON
    res.body = dumps({"id": row_id, "randomNumber": number})


async def json(req: Request, res: Response, ctx: Context):
    res.headers = CONTENT_TYPE, APPLICATION_JSON
    res.body = dumps({'message': 'Hello, World!'})


async def queries(req: Request, res: Response, ctx: Context):
    pool = req.app.peek(POOL)
    queries = req.params.get('queries')
    num_queries = get_num_queries(queries)
    row_ids = sample(range(1, 10001), num_queries)
    worlds = []

    async with pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append({"id": row_id, "randomNumber": number})

    res.headers = CONTENT_TYPE, APPLICATION_JSON
    res.body = dumps(worlds)


async def fortunes(req: Request, res: Response, ctx: Context):
    pool = req.app.peek(POOL)
    async with pool.acquire() as connection:
        fortunes = await connection.fetch("SELECT * FROM Fortune")

    fortunes.append([0, 'Additional fortune added at request time.'])
    fortunes.sort(key=lambda row: row[1])
    await res.render("fortune.html", fortunes=fortunes, request=req)


async def updates(req: Request, res: Response, ctx: Context):
    pool = req.app.peek(POOL)
    queries = req.params.get('queries')
    num_queries = get_num_queries(queries)

    # To avoid deadlock
    ids = sorted(sample(range(1, 10000 + 1), num_queries))
    numbers = sorted(sample(range(1, 10000), num_queries))
    updates = list(zip(ids, numbers))

    worlds = [
        {"id": row_id, "randomNumber": number} for row_id, number in updates
    ]

    async with pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id, _ in updates:
            await statement.fetchval(row_id)
        await connection.executemany(WRITE_ROW_SQL, updates)
    res.headers = CONTENT_TYPE, APPLICATION_JSON
    res.body = dumps(worlds)


async def plaintext(req: Request, res: Response, ctx: Context):
    res.headers = 'Content-Type', 'text/plain'
    res.body = b"Hello, World!"


################
# App creation #
################
app = Application()
app.TEMPLATES('templates')


###################
# Register hooks  #
###################
app.ON(STARTUP, up_database)
app.ON(SHUTDOWN, down_database)


###################
# Register routes #
###################
app.GET('/db', database)
app.GET('/queries', queries)
app.GET('/fortunes', fortunes)
app.GET('/updates', updates)
app.GET('/plaintext', plaintext)
app.GET('/json', json)
