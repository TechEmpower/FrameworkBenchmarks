#!/usr/bin/env python3
import random

import asyncpg
from quart import Quart, jsonify, make_response, request, render_template

app = Quart(__name__)


@app.before_serving
async def connect_to_db():
    app.db = await asyncpg.connect(
        "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world"
    )
    app.get_world = await app.db.prepare("select * from world where id = $1")
    app.update_world = await app.db.prepare(
        "update world set randomNumber = $2 where id = $1"
    )


@app.route("/json")
def json():
    return jsonify(message="Hello, World!")


@app.route("/plaintext")
async def plaintext():
    response = await make_response(b"Hello, World!")
    # Quart assumes string responses are 'text/html', so make a custom one
    response.mimetype = "text/plain"
    return response


async def get_random_world():
    key = random.randint(1, 10000)
    row = await app.get_world.fetchrow(key)
    return {"id": row[0], "randomNumber": row[1]}


@app.route("/db")
async def db():
    return jsonify(await get_random_world())


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

    async with app.db.transaction():
        return jsonify([await get_random_world() for _ in range(queries)])


@app.route("/updates")
async def updates():
    queries = get_query_count(request.args)

    worlds = []
    async with app.db.transaction():
        for _ in range(queries):
            world = await get_random_world()
            new_number = random.randint(1, 10000)
            await app.update_world.fetch(world["id"], new_number)
            world["randomNumber"] = new_number
            worlds.append(world)

    return jsonify(worlds)


@app.route("/fortunes")
async def fortunes():
    async with app.db.transaction():
        rows = [
            (f["id"], f["message"])
            async for f in app.db.cursor("select * from fortune")
        ]
    rows.append((0, "Additional fortune added at request time."))
    rows.sort(key=lambda row: row[1])

    return await render_template("fortunes.html", fortunes=rows)


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8080)
