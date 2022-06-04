import asyncio
import asyncpg
import os
import jinja2
from fastapi import FastAPI
from fastapi.responses import HTMLResponse, ORJSONResponse, PlainTextResponse
from random import randint, sample
from operator import itemgetter
from functools import partial

READ_ROW_SQL = 'SELECT "id", "randomnumber" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, "Additional fortune added at request time."]


async def setup_database():
    global connection_pool
    connection_pool = await asyncpg.create_pool(
        user=os.getenv("PGUSER", "benchmarkdbuser"),
        password=os.getenv("PGPASS", "benchmarkdbpass"),
        database="hello_world",
        host="tfb-database",
        port=5432,
    )


def load_fortunes_template():
    path = os.path.join("templates", "fortune.html")
    with open(path, "r") as template_file:
        template_text = template_file.read()
        return jinja2.Template(template_text)


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
template = load_fortunes_template()
loop = asyncio.get_event_loop()
loop.run_until_complete(setup_database())


app = FastAPI()


@app.get("/json")
async def json_serialization():
    return ORJSONResponse({"message": "Hello, world!"})


@app.get("/db")
async def single_database_query():
    async with connection_pool.acquire() as connection:
        record = await connection.fetchrow(READ_ROW_SQL, randint(1, 10000))

    return ORJSONResponse({"id": record['id'], "randomNumber": record['randomnumber']})


@app.get("/queries")
async def multiple_database_queries(queries=None):
    num_queries = get_num_queries(queries)
    row_ids = sample(range(1, 10000), num_queries)
    worlds = []

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append({'id': row_id, 'randomNumber': number})

    return ORJSONResponse(worlds)


@app.get("/fortunes")
async def fortunes():
    async with connection_pool.acquire() as connection:
        fortunes = await connection.fetch("SELECT * FROM Fortune")

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=sort_fortunes_key)
    content = template.render(fortunes=fortunes)
    return HTMLResponse(content)


@app.get("/updates")
async def database_updates(queries=None):
    num_queries = get_num_queries(queries)
    updates = [(row_id, randint(1, 10000)) for row_id in sample(range(1, 10000), num_queries)]
    worlds = [{"id": row_id, "randomNumber": number} for row_id, number in updates]

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id, number in updates:
            await statement.fetchval(row_id)
        await connection.executemany(WRITE_ROW_SQL, updates)

    return ORJSONResponse(worlds)


@app.get("/plaintext")
async def plaintext():
    return PlainTextResponse(b"Hello, world!")
