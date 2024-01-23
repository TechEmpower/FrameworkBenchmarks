import multiprocessing
from contextlib import asynccontextmanager

import asyncpg
import os
from fastapi import FastAPI, Request
from fastapi.responses import PlainTextResponse

try:
    import orjson
    from fastapi.responses import ORJSONResponse as JSONResponse
except ImportError:
    from fastapi.responses import UJSONResponse as JSONResponse

from fastapi.templating import Jinja2Templates
from random import randint, sample

READ_ROW_SQL = 'SELECT "id", "randomnumber" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, "Additional fortune added at request time."]
MAX_POOL_SIZE = 1000//multiprocessing.cpu_count()
MIN_POOL_SIZE = max(int(MAX_POOL_SIZE / 2), 1)


def get_num_queries(queries):
    try:
        query_count = int(queries)
    except (ValueError, TypeError):
        return 1

    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


connection_pool = None

templates = Jinja2Templates(directory="templates")


async def setup_database():
    return await asyncpg.create_pool(
        user=os.getenv("PGUSER", "benchmarkdbuser"),
        password=os.getenv("PGPASS", "benchmarkdbpass"),
        database="hello_world",
        host="tfb-database",
        port=5432,
        min_size=MIN_POOL_SIZE,
        max_size=MAX_POOL_SIZE,
    )


@asynccontextmanager
async def lifespan(app: FastAPI):
    # Setup the database connection pool
    app.state.connection_pool = await setup_database()
    yield
    # Close the database connection pool
    await app.state.connection_pool.close()


app = FastAPI(lifespan=lifespan)


@app.get("/json")
async def json_serialization():
    return JSONResponse({"message": "Hello, world!"})


@app.get("/db")
async def single_database_query():
    row_id = randint(1, 10000)
    async with app.state.connection_pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)

    return JSONResponse({"id": row_id, "randomNumber": number})


@app.get("/queries")
async def multiple_database_queries(queries = None):
    num_queries = get_num_queries(queries)
    row_ids = sample(range(1, 10000), num_queries)
    worlds = []

    async with app.state.connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append({"id": row_id, "randomNumber": number})

    return JSONResponse(worlds)


@app.get("/fortunes")
async def fortunes(request: Request):
    async with app.state.connection_pool.acquire() as connection:
        fortunes = await connection.fetch("SELECT * FROM Fortune")

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=lambda row: row[1])
    return templates.TemplateResponse("fortune.html", {"fortunes": fortunes, "request": request})


@app.get("/updates")
async def database_updates(queries = None):
    num_queries = get_num_queries(queries)
    # To avoid deadlock
    ids = sorted(sample(range(1, 10000 + 1), num_queries))
    numbers = sorted(sample(range(1, 10000), num_queries))
    updates = list(zip(ids, numbers))

    worlds = [
        {"id": row_id, "randomNumber": number} for row_id, number in updates
    ]

    async with app.state.connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id, _ in updates:
            await statement.fetchval(row_id)
        await connection.executemany(WRITE_ROW_SQL, updates)

    return JSONResponse(worlds)


@app.get("/plaintext")
async def plaintext():
    return PlainTextResponse(b"Hello, world!")
