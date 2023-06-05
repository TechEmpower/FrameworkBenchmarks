#!/usr/bin/env python
from datetime import datetime
import os
from random import randint, sample

import asyncpg
from asyncache import cached
from cachetools.keys import hashkey

from microdot_asgi import Microdot
from microdot_jinja import render_template

app = Microdot()
get_world_sql = 'SELECT id, randomnumber FROM world WHERE id = $1'
update_world_sql = 'UPDATE world SET randomnumber = $1 WHERE id = $2'
fortune_sql = 'SELECT * FROM fortune'
db = None

async def asgi(scope, receive, send):
    if scope['type'] == 'lifespan':
        while True:
            message = await receive()
            if message['type'] == 'lifespan.startup':
                global db, get_world_stmt, update_world_stmt, fortune_stmt
                db = await asyncpg.create_pool(os.environ['DATABASE_URL'])
                await send({'type': 'lifespan.startup.complete'})
            elif message['type'] == 'lifespan.shutdown':
                db.close()
                await send({'type': 'lifespan.shutdown.complete'})
                return
    else:
        return await app(scope, receive, send)


def get_num_queries(request, name="queries"):
    try:
        num_queries = request.args.get(name, 1, type=int)
    except ValueError:
        num_queries = 1
    if num_queries < 1:
        return 1
    if num_queries > 500:
        return 500
    return num_queries


def generate_ids(num_queries):
    return sample(range(1, 10001), num_queries)


@app.route("/json")
async def test_json(request):
    return {"message": "Hello, World!"}


@app.route("/db")
async def test_db(request):
    id = randint(1, 10000)
    async with db.acquire() as conn:
        result = await conn.fetchrow(get_world_sql, id)
        world = {'id': result[0], 'randomNumber': result[1]}
    return world


async def get_world(stmt, id):
    result = await stmt.fetchrow(id)
    return {'id': result[0], 'randomNumber': result[1]}


@app.route("/queries")
async def test_queries(request):
    async with db.acquire() as conn:
        stmt = await conn.prepare(get_world_sql)
        worlds = [await get_world(stmt, id) for id in generate_ids(get_num_queries(request))]
    return worlds


@app.route("/fortunes")
async def test_fortunes(request):
    async with db.acquire() as conn:
        fortunes = list(await conn.fetch(fortune_sql))
    fortunes.append((0, "Additional fortune added at request time."))
    fortunes.sort(key=lambda f: f[1])
    return render_template("fortunes_raw.html", fortunes=fortunes), {'Content-Type': 'text/html; charset=utf-8'}


@app.route("/updates")
async def test_updates(request):
    worlds = []
    updated_worlds = []
    ids = generate_ids(get_num_queries(request))
    ids.sort()  # to avoid deadlocks
    async with db.acquire() as conn:
        get_stmt = await conn.prepare(get_world_sql)
        update_stmt = await conn.prepare(update_world_sql)
        for id in ids:
            world = await get_world(get_stmt, id)
            new_value = randint(1, 10000)
            updated_worlds.append((new_value, id))
            worlds.append({'id': id, 'randomNumber': new_value})
        await update_stmt.executemany(updated_worlds)
    return worlds


@app.route("/plaintext")
async def test_plaintext(request):
    return b"Hello, World!"


@cached(cache={}, key=lambda stmt, id: hashkey(id))
async def get_cached_world(stmt, id):
    result = await stmt.fetchrow(id)
    return {'id': result[0], 'randomNumber': result[1]}


@app.route("/cached-queries")
async def test_cached_queries(request):
    async with db.acquire() as conn:
        get_stmt = await conn.prepare(get_world_sql)
        worlds = [await get_cached_world(get_stmt, id) for id in generate_ids(get_num_queries(request, 'count'))]
    return worlds
