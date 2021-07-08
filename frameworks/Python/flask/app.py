#!/usr/bin/env python
from collections import namedtuple
from operator import attrgetter
from random import randint
import sys

from flask import Flask, request, render_template, make_response, jsonify
from pony import orm

if sys.version_info[0] == 3:
    xrange = range
_is_pypy = hasattr(sys, 'pypy_version_info')
if _is_pypy:
    from psycopg2cffi import compat
    compat.register()

DBDRIVER = 'postgres'
DBHOST = 'tfb-database'

# setup

app = Flask(__name__)
app.config['STORM_DATABASE_URI'] = "{DBDRIVER}://benchmarkdbuser:benchmarkdbpass@{DBHOST}:5432/hello_world".format(DBDRIVER=DBDRIVER, DBHOST=DBHOST)
app.config['JSONIFY_PRETTYPRINT_REGULAR'] = False
db = orm.Database()
db.bind(DBDRIVER, host=DBHOST, port=5432, user="benchmarkdbuser", password="benchmarkdbpass", database="hello_world")


class World(db.Entity):
    _table_ = "world"
    id = orm.PrimaryKey(int)
    randomNumber = orm.Required(int, column="randomnumber")

    def to_dict(self):
        """Return object data in easily serializeable format"""
        return {
            'id'         : self.id,
            'randomNumber': self.randomNumber
        }


class Fortune(db.Entity):
    _table_ = "fortune"
    id = orm.PrimaryKey(int, auto=True)
    message = orm.Required(str)


db.generate_mapping(create_tables=False)


def get_num_queries():
    try:
        num_queries = request.args.get("queries", 1, type=int)
    except ValueError:
        num_queries = 1
    if num_queries < 1:
        return 1
    if num_queries > 500:
        return 500
    return num_queries


def generate_ids(num_queries):
    ids = {randint(1, 10000) for _ in xrange(num_queries)}
    while len(ids) < num_queries:
        ids.add(randint(1, 10000))
    return list(sorted(ids))


@app.route("/json")
def hello():
    return jsonify(message='Hello, World!')


@app.route("/query")
def get_random_world():
    with orm.db_session(serializable=False):
        worlds = [World[ident].to_dict()
              for ident in generate_ids(get_num_queries())]
    return jsonify(worlds)


@app.route("/db")
def get_random_world_single():
    wid = randint(1, 10000)
    with orm.db_session(serializable=False):
        world = World[wid]
    return jsonify(world.to_dict())


@app.route("/fortunes")
def get_fortunes():
    with orm.db_session(serializable=False):
        fortunes = list(orm.select(fortune for fortune in Fortune))
    tmp_fortune = namedtuple("Fortune", ["id", "message"])
    fortunes.append(tmp_fortune(id=0, message="Additional fortune added at request time."))
    fortunes.sort(key=attrgetter('message'))
    return render_template('fortunes.html', fortunes=fortunes)


@app.route("/updates")
def updates():
    """Test 5: Database Updates"""
    num_queries = get_num_queries()
    ids = generate_ids(num_queries)
    ids.sort()
    worlds = []
    with orm.db_session(serializable=False):
        for ident in ids:
            world = World[ident]
            world.randomNumber = randint(1, 10000)
            worlds.append({"id": world.id, "randomNumber": world.randomNumber})
    return jsonify(worlds)


@app.route('/plaintext')
def plaintext():
    """Test 6: Plaintext"""
    response = make_response(b'Hello, World!')
    response.content_type = 'text/plain'
    return response


try:
    import meinheld
    meinheld.server.set_access_logger(None)
    meinheld.set_keepalive(120)
except ImportError:
    pass

# entry point for debugging
if __name__ == "__main__":
    app.run(debug=True)
