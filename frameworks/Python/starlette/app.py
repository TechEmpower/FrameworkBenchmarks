import asyncpg
import os
from starlette.applications import Starlette
from starlette.responses import HTMLResponse, JSONResponse, PlainTextResponse
from starlette.routing import Route
from starlette.templating import Jinja2Templates
from random import randint, sample


READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'



async def setup_database():
    global connection_pool
    connection_pool = await asyncpg.create_pool(
        user=os.getenv('PGUSER', 'benchmarkdbuser'),
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        database='hello_world',
        host='tfb-database',
        port=5432
    )


def get_num_queries(request):
    try:
        query_count = int(request.query_params["queries"])
    except (KeyError, IndexError, ValueError):
        return 1

    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


connection_pool = None
templates = Jinja2Templates(directory="templates")


async def single_database_query(request):
    row_id = randint(1, 10000)

    async with connection_pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)

    return JSONResponse({'id': row_id, 'randomNumber': number})


async def multiple_database_queries(request):
    num_queries = get_num_queries(request)
    row_ids = sample(range(1, 10000), num_queries)
    worlds = []

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append({'id': row_id, 'randomNumber': number})

    return JSONResponse(worlds)


async def fortunes(request):
    async with connection_pool.acquire() as connection:
        fortunes = await connection.fetch('SELECT * FROM Fortune')

    fortunes.append([0, 'Additional fortune added at request time.'])
    fortunes.sort(key=lambda row: row[1])
    return templates.TemplateResponse("fortune.html", {"fortunes": fortunes, "request": request})


async def database_updates(request):
    num_queries = get_num_queries(request)
    updates = [(row_id, randint(1, 10000)) for row_id in sample(range(1, 10000), num_queries)]
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id, number in updates:
            await statement.fetchval(row_id)
        await connection.executemany(WRITE_ROW_SQL, updates)

    return JSONResponse(worlds)


routes = [
    Route('/json', JSONResponse({'message': 'Hello, world!'})),
    Route('/db', single_database_query),
    Route('/queries', multiple_database_queries),
    Route('/fortunes', fortunes),
    Route('/updates', database_updates),
    Route('/plaintext', PlainTextResponse(b'Hello, world!')),
]

app = Starlette(routes=routes, on_startup=[setup_database])
