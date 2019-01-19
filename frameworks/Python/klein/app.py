# -*- coding: utf-8 -*-

from functools import partial, wraps
import json
from operator import attrgetter
import os
from random import randint
import sys

from jinja2 import Environment, PackageLoader
from klein import Klein, run, route
from sqlalchemy import create_engine, Column
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
from sqlalchemy.types import String, Integer, Unicode

if sys.version_info[0] == 3:
    xrange = range

DATABASE_URI = 'mysql://benchmarkdbuser:benchmarkdbpass@tfb-database:3306/hello_world?charset=utf8'

Base = declarative_base()
db_engine = create_engine(DATABASE_URI)
Session = sessionmaker(bind=db_engine)
db_session = Session()

env = Environment(
    loader=PackageLoader("app", "templates"),
    autoescape=True,
    auto_reload=False)

app = Klein()


class Fortune(Base):
    __tablename__ = "Fortune"
    id = Column(Integer, primary_key=True)
    message = Column(String)

    def serialize(self):
        return {
            'id': self.id,
            'randomNumber': self.randomNumber,
        }


class World(Base):
    __tablename__ = "World"
    id = Column(Integer, primary_key=True)
    randomNumber = Column(Integer)

    def serialize(self):
        return {
            'id': self.id,
            'randomNumber': self.randomNumber,
        }


def getQueryNum(queryString):
    try:
        num_queries = int(queryString)
        if num_queries < 1:
            return 1
        if num_queries > 500:
            return 500
        return num_queries
    except ValueError:
        return 1


def close_session(func):
    @wraps(func)
    def wrapper(request):
        try:
            return func(request)
        finally:
            db_session.close()

    return wrapper


@app.route("/plaintext")
def plaintext(request):
    request.setHeader("Content-Type", "text/plain; charset=UTF-8")
    return "Hello, World!"


@app.route("/json")
def jsonHandler(request):
    request.setHeader("Content-Type", "application/json; charset=UTF-8")
    return json.dumps({"message": "Hello, World!"})


@app.route("/db")
@close_session
def db(request):
    request.setHeader("Content-Type", "application/json; charset=UTF-8")
    wid = randint(1, 10000)
    world = db_session.query(World).get(wid).serialize()
    return json.dumps(world)


@app.route("/queries")
@close_session
def queries(request):
    request.setHeader("Content-Type", "application/json; charset=UTF-8")
    num_queries = getQueryNum(request.args.get("queries")[0])
    rp = partial(randint, 1, 10000)
    get = db_session.query(World).get
    worlds = [get(rp()).serialize() for _ in xrange(num_queries)]
    return json.dumps(worlds)


@app.route("/updates")
@close_session
def updates(request):
    request.setHeader("Content-Type", "application/json; charset=UTF-8")
    num_queries = getQueryNum(request.args.get("queries")[0])
    worlds = []
    rp = partial(randint, 1, 10000)
    ids = [rp() for _ in xrange(num_queries)]
    ids.sort()
    for id in ids:
        world = db_session.query(World).get(id)
        world.randomNumber = rp()
        worlds.append(world.serialize())
    db_session.commit()
    return json.dumps(worlds)


@app.route("/fortune")
@close_session
def fortune(request):
    request.setHeader("Content-Type", "text/html; charset=UTF-8")
    fortunes = db_session.query(Fortune).all()
    fortunes.append(
        Fortune(id=0, message="Additional fortune added at request time."))
    fortunes.sort(key=attrgetter("message"))
    template = env.get_template("fortunes.html")
    return template.render(fortunes=fortunes)


if __name__ == "__main__":
    app.run("0.0.0.0", 8080)

# vim: set expandtab sw=4 sts=4 ts=4 :
