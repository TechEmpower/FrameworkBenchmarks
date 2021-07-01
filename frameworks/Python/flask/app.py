#!/usr/bin/env python
from operator import attrgetter
from random import randint
import sys

from flask import Flask, request, render_template, make_response, jsonify
from flask_sqlalchemy import SQLAlchemy
from sqlalchemy_serializer import SerializerMixin
if sys.version_info[0] == 3:
    xrange = range
_is_pypy = hasattr(sys, 'pypy_version_info')
if _is_pypy:
    from psycopg2cffi import compat
    compat.register()

DBDRIVER = 'postgresql'
DBHOST = 'tfb-database'

# setup

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = "{DBDRIVER}://benchmarkdbuser:benchmarkdbpass@{DBHOST}:5432/hello_world".format(DBDRIVER=DBDRIVER, DBHOST=DBHOST)
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
app.config['SQLALCHEMY_ECHO'] = False
app.config['JSONIFY_PRETTYPRINT_REGULAR'] = False
db = SQLAlchemy(app)


# models

class World(db.Model, SerializerMixin):
    __tablename__ = "world"
    id = db.Column(db.Integer, primary_key=True)
    randomNumber = db.Column(db.Integer, name="randomnumber")

    # http://stackoverflow.com/questions/7102754/jsonify-a-sqlalchemy-result-set-in-flask
    @property
    def serialize(self):
        """Return object data in easily serializeable format"""
        return {
            'id'         : self.id,
            'randomNumber': self.randomNumber
        }


class Fortune(db.Model, SerializerMixin):
    __tablename__ = "fortune"
    id = db.Column(db.Integer, primary_key=True)
    message = db.Column(db.String)


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
    worlds = [World.query.get(ident).to_dict()
              for ident in generate_ids(get_num_queries())]
    return jsonify(worlds)


@app.route("/db")
def get_random_world_single():
    wid = randint(1, 10000)
    world = World.query.get(wid)
    return jsonify(world.to_dict())


@app.route("/fortunes")
def get_fortunes():
    fortunes = list(Fortune.query.all())
    fortunes.append(Fortune(id=0, message="Additional fortune added at request time."))
    fortunes.sort(key=attrgetter('message'))
    return render_template('fortunes.html', fortunes=fortunes)


@app.route("/updates")
def updates():
    """Test 5: Database Updates"""
    num_queries = get_num_queries()
    ids = generate_ids(num_queries+1)
    updates = generate_ids(num_queries+1)
    worlds = tuple({"id": ident, "randomNumber": update} for ident, update in zip(ids[:-1], updates[:-1]))
    for ident, update in zip(ids, updates):
        world = World.query.get(ident)
        world.randomNumber = update
        db.session.commit()
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
