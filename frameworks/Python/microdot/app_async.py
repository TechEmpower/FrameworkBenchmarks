#!/usr/bin/env python
from datetime import datetime
import os
from random import randint, sample

from alchemical.aio import Alchemical
import sqlalchemy as sqla
from asyncache import cached
from cachetools.keys import hashkey

from microdot_asgi import Microdot
from microdot_jinja import render_template

app = Microdot()
db = Alchemical(os.environ['DATABASE_URL'])


class World(db.Model):
    __tablename__ = "world"
    id = sqla.Column(sqla.Integer, primary_key=True)
    randomnumber = sqla.Column(sqla.Integer)

    def to_dict(self):
        return {"id": self.id, "randomNumber": self.randomnumber}


class CachedWorld(db.Model):
    __tablename__ = "cachedworld"
    id = sqla.Column(sqla.Integer, primary_key=True)
    randomnumber = sqla.Column(sqla.Integer)

    def to_dict(self):
        return {"id": self.id, "randomNumber": self.randomnumber}


class Fortune(db.Model):
    __tablename__ = "fortune"
    id = sqla.Column(sqla.Integer, primary_key=True)
    message = sqla.Column(sqla.String)


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
    async with db.Session() as session:
        world = await session.get(World, id)
    return world.to_dict()


@app.route("/queries")
async def test_queries(request):
    async with db.Session() as session:
        worlds = [(await session.get(World, id)).to_dict() for id in generate_ids(get_num_queries(request))]
    return worlds


@app.route("/fortunes")
async def test_fortunes(request):
    async with db.Session() as session:
        fortunes = list(await session.scalars(Fortune.select()))
    fortunes.append(Fortune(id=0, message="Additional fortune added at request time."))
    fortunes.sort(key=lambda f: f.message)
    return render_template("fortunes.html", fortunes=fortunes), {'Content-Type': 'text/html; charset=utf-8'}


@app.route("/updates")
async def test_updates(request):
    worlds = []
    ids = generate_ids(get_num_queries(request))
    ids.sort()  # to avoid deadlocks
    async with db.begin() as session:
        for id in ids:
            world = await session.get(World, id)
            world.randomnumber = (randint(1, 9999) + world.randomnumber - 1) % 10000 + 1
            worlds.append(world.to_dict())
    return worlds


@app.route("/plaintext")
async def test_plaintext(request):
    return b"Hello, World!"


@cached(cache={}, key=lambda session, id: hashkey(id))
async def get_cached_world(session, id):
    return (await session.get(World, id)).to_dict()


@app.route("/cached-queries")
async def test_cached_queries(request):
    async with db.Session() as session:
        worlds = [await get_cached_world(session, id) for id in generate_ids(get_num_queries(request, 'count'))]
    return worlds
