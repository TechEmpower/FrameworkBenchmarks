#!/usr/bin/env python3
import random
import os

import asyncpg
from quart import Quart, jsonify, make_response, request, render_template

app = Quart(__name__)

GET_WORLD = "select id,randomnumber from world where id = $1"
UPDATE_WORLD = "update world set randomNumber = $2 where id = $1"


@app.before_serving
async def connect_to_db():
    app.db = await asyncpg.create_pool(
        user=os.getenv("PGUSER", "benchmarkdbuser"),
        password=os.getenv("PGPASS", "benchmarkdbpass"),
        database="hello_world",
        host="tfb-database",
        port=5432,
    )


@app.after_serving
async def disconnect_from_db():
    await app.db.close()


@app.route("/json")
async def json():
    return {"message": "Hello, World!"}


@app.route("/plaintext")
async def plaintext():
    response = await make_response(b"Hello, World!")
    # Quart assumes string responses are 'text/html', so make a custom one
    response.mimetype = "text/plain"
    return response


@app.route("/db")
async def db():
    async with app.db.acquire() as conn:
        key = random.randint(1, 10000)
        number = await conn.fetchval(GET_WORLD, key)
        return jsonify({"id": key, "randomNumber": number})


def get_query_count(args):
    qc = args.get("queries")

    if qc is None:
        return 1

    try:
        qc = int(qc)
    except ValueError:
        return 1

    qc = max(qc, 1)
    qc = min(qc, 500)
    return qc


@app.route("/queries")
async def queries():
    queries = get_query_count(request.args)

    worlds = []
    async with app.db.acquire() as conn:
        pst = await conn.prepare(GET_WORLD)
        for _ in range(queries):
            key = random.randint(1, 10000)
            number = await pst.fetchval(key)
            worlds.append({"id": key, "randomNumber": number})

    return jsonify(worlds)


@app.route("/updates")
async def updates():
    queries = get_query_count(request.args)

    new_worlds = []
    async with app.db.acquire() as conn, conn.transaction():
        pst = await conn.prepare(GET_WORLD)

        for _ in range(queries):
            key = random.randint(1, 10000)
            old_number = await pst.fetchval(key)
            new_number = random.randint(1, 10000)
            new_worlds.append((key, new_number))

        await conn.executemany(UPDATE_WORLD, new_worlds)

    return jsonify(
        [{"id": key, "randomNumber": new_number} for key, new_number in new_worlds]
    )


@app.route("/fortunes")
async def fortunes():
    async with app.db.acquire() as conn:
        rows = await conn.fetch("select * from fortune")
    rows.append((0, "Additional fortune added at request time."))
    rows.sort(key=lambda row: row[1])

    return await render_template("fortunes.html", fortunes=rows)


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8080)
