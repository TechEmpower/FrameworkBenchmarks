import asyncio
import asyncpg
import os
from fastapi import FastAPI, Request
from fastapi.templating import Jinja2Templates
from starlette.responses import HTMLResponse, JSONResponse, PlainTextResponse
from random import randint, sample
from operator import itemgetter

READ_ROW_SQL = 'SELECT "id", "randomnumber" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, "Additional fortune added at request time."]


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
sort_fortunes_key = itemgetter(1)

app = FastAPI()

templates = Jinja2Templates(directory="templates")


@app.on_event("startup")
async def setup_database():
    global connection_pool
    connection_pool = await asyncpg.create_pool(
        user=os.getenv("PGUSER", "benchmarkdbuser"),
        password=os.getenv("PGPASS", "benchmarkdbpass"),
        database="hello_world",
        host="tfb-database",
        port=5432,
    )


@app.get("/json")
async def json_serialization():
    return JSONResponse({"message": "Hello, world!"})


@app.get("/db")
async def single_database_query():
    row_id = randint(1, 10000)
    async with connection_pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)

    return JSONResponse({"id": row_id, "randomNumber": number})


@app.get("/queries")
async def multiple_database_queries(queries = None):
    num_queries = get_num_queries(queries)
    row_ids = sample(range(1, 10000), num_queries)
    worlds = []

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append({"id": row_id, "randomNumber": number})

    return JSONResponse(worlds)


@app.get("/fortunes")
async def fortunes(request: Request):
    async with connection_pool.acquire() as connection:
        fortunes = await connection.fetch("SELECT * FROM Fortune")

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=sort_fortunes_key)
    return templates.TemplateResponse("fortune.html", {"fortunes": fortunes, "request": request})


@app.get("/updates")
async def database_updates(queries = None):
    num_queries = get_num_queries(queries)
    updates = [(row_id, randint(1, 10000)) for row_id in sample(range(1, 10000), num_queries)]
    worlds = [{"id": row_id, "randomNumber": number} for row_id, number in updates]

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id, number in updates:
            await statement.fetchval(row_id)
        await connection.executemany(WRITE_ROW_SQL, updates)

    return JSONResponse(worlds)


@app.get("/plaintext")
async def plaintext():
    return PlainTextResponse(b"Hello, world!")
