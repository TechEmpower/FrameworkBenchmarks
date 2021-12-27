import multiprocessing
import os
from operator import itemgetter
from random import randint, sample

import asyncpg

from fastapi import FastAPI, Request
from fastapi.responses import PlainTextResponse, ORJSONResponse
from fastapi.templating import Jinja2Templates

ADDITIONAL_FORTUNE = [0, "Additional fortune added at request time."]
READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$2 WHERE id=$1'

sort_fortunes_key = itemgetter(1)

template_path = os.path.join(
    os.path.dirname(os.path.realpath(__file__)), "templates"
)
templates = Jinja2Templates(directory=template_path)

app = FastAPI()


async def setup_database():
    max_size = min(1800 / multiprocessing.cpu_count(), 160)
    max_size = max(int(max_size), 1)
    min_size = max(int(max_size / 2), 1)

    return await asyncpg.create_pool(
        user=os.getenv("PGUSER", "benchmarkdbuser"),
        password=os.getenv("PGPASS", "benchmarkdbpass"),
        database="hello_world",
        host="tfb-database",
        port=5432,
        min_size=min_size,
        max_size=max_size,
        ssl=False,
    )


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


@app.on_event("startup")
async def startup_event():
    app.state.connection_pool = await setup_database()


@app.on_event("shutdown")
async def shutdown_event():
    await app.state.connection_pool.close()


@app.get("/json")
async def json_serialization():
    return ORJSONResponse({"message": "Hello, world!"})


@app.get("/db")
async def single_database_query():
    row_id = randint(1, 10000)

    async with app.state.connection_pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)

    return ORJSONResponse({"id": row_id, "randomNumber": number})


@app.get("/queries")
async def multiple_database_queries(queries=None):
    num_queries = get_num_queries(queries)
    row_ids = sorted(sample(range(1, 10000), num_queries))
    data = []

    async with app.state.connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            data.append({"id": row_id, "randomNumber": number})

    return ORJSONResponse(data)


@app.get("/fortunes")
async def fortunes(request: Request):
    async with app.state.connection_pool.acquire() as connection:
        data = await connection.fetch("SELECT * FROM Fortune")

    data.append(ADDITIONAL_FORTUNE)
    data.sort(key=sort_fortunes_key)
    return templates.TemplateResponse(
        "fortune.html", {"request": request, "fortunes": data}
    )


@app.get("/updates")
async def database_updates(queries=None):
    num_queries = get_num_queries(queries)

    ids = sorted(sample(range(1, 10000 + 1), num_queries))
    numbers = sample(range(1, 10000), num_queries)
    updates = list(zip(ids, numbers))

    data = [
        {"id": row_id, "randomNumber": number} for row_id, number in updates
    ]

    async with app.state.connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id, _ in updates:
            await statement.fetchval(row_id)

        await connection.executemany(WRITE_ROW_SQL, updates)

    return ORJSONResponse(data)


@app.get("/plaintext")
async def plaintext():
    return PlainTextResponse(b"Hello, world!")
