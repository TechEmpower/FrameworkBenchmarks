#!/usr/bin/env python3
from random import randint, sample
import os
import os.path
from typing import Any, Dict, List, Tuple
from urllib.parse import parse_qs

import asyncpg
import jinja2
import orjson

from bareasgi import Application, HttpRequest, HttpResponse, LifespanRequest
from bareasgi_jinja2 import add_jinja2, Jinja2TemplateProvider

GET_WORLD = "SELECT id, randomnumber FROM world WHERE id = $1"
UPDATE_WORLD = "UPDATE world SET randomNumber = $2 WHERE id = $1"
GET_FORTUNES = "SELECT * FROM fortune"
ADDITIONAL_ROW = (0, "Additional fortune added at request time.")


async def on_startup(_request: LifespanRequest) -> None:
    app.info['db'] = await asyncpg.create_pool(
        user=os.getenv("PGUSER", "benchmarkdbuser"),
        password=os.getenv("PGPASS", "benchmarkdbpass"),
        database="hello_world",
        host="tfb-database",
        port=5432,
    )


async def on_shutdown(_request: LifespanRequest) -> None:
    await app.info['db'].close()


async def handle_json_request(_request: HttpRequest) -> HttpResponse:
    return HttpResponse.from_json(
        {"message": "Hello, World!"},
        encode_bytes=orjson.dumps
    )


async def handle_plaintext_request(_request: HttpRequest) -> HttpResponse:
    return HttpResponse.from_text("Hello, World!")


async def handle_db_request(_request: HttpRequest) -> HttpResponse:
    key = randint(1, 10000)
    async with app.info['db'].acquire() as conn:
        number = await conn.fetchval(GET_WORLD, key)
    return HttpResponse.from_json({"id": key, "randomNumber": number})


def get_query_count(request: HttpRequest):
    try:
        qs = parse_qs(request.scope["query_string"])
        num_queries = int(qs.get(b'queries', (1, ))[0])
    except ValueError:
        num_queries = 1
    if num_queries < 1:
        return 1
    if num_queries > 500:
        return 500
    return num_queries


async def handle_queries_request(request: HttpRequest) -> HttpResponse:
    queries = get_query_count(request)

    worlds: List[Dict[str, Any]] = []
    async with app.info['db'].acquire() as conn:
        pst = await conn.prepare(GET_WORLD)
        for key in sample(range(1, 10000), queries):
            number = await pst.fetchval(key)
            worlds.append({"id": key, "randomNumber": number})

    return HttpResponse.from_json(worlds)


async def handle_updates_request(request: HttpRequest) -> HttpResponse:
    queries = get_query_count(request)
    updates = [(row_id, randint(1, 10000)) for row_id in sample(range(1, 10000), queries)]
    updates.sort()
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]

    async with app.info['db'].acquire() as connection:
        statement = await connection.prepare(GET_WORLD)
        for row_id, number in updates:
            await statement.fetchval(row_id)
        await connection.executemany(UPDATE_WORLD, updates)

    return HttpResponse.from_json(
        worlds,
        encode_bytes=orjson.dumps
    )

async def handle_fortunes_request(request: HttpRequest) -> HttpResponse:
    async with app.info['db'].acquire() as conn:
        rows = await conn.fetch(GET_FORTUNES)
    rows.append(ADDITIONAL_ROW)
    rows.sort(key=lambda row: row[1])

    return await Jinja2TemplateProvider.apply(
        request,
        "fortune.html",
        { "fortunes": rows }
    )

app = Application(
    startup_handlers=[on_startup],
    shutdown_handlers=[on_shutdown]
)

here = os.path.abspath(os.path.dirname(__file__))
env = jinja2.Environment(
    loader=jinja2.FileSystemLoader(os.path.join(here, 'templates')),
    autoescape=jinja2.select_autoescape(['html', 'xml']),
    enable_async=True
)

add_jinja2(app, env)

app.http_router.add({"GET"}, "/json", handle_json_request)
app.http_router.add({"GET"}, "/plaintext", handle_plaintext_request)
app.http_router.add({"GET"}, "/db", handle_db_request)
app.http_router.add({"GET"}, "/queries", handle_queries_request)
app.http_router.add({"GET"}, "/updates", handle_updates_request)
app.http_router.add({"GET"}, "/fortunes", handle_fortunes_request)
