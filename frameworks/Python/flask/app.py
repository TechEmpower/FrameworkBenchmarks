#!/usr/bin/env python
from functools import partial
import json
from operator import attrgetter
import os
from random import randint
import sys

import flask
from flask import Flask, request, render_template, make_response, jsonify
from flask_sqlalchemy import SQLAlchemy
from sqlalchemy import create_engine
from sqlalchemy.ext import baked

if sys.version_info[0] == 3:
    xrange = range

_is_pypy = hasattr(sys, 'pypy_version_info')

DBDRIVER = 'mysql+pymysql' if _is_pypy else 'mysql'  # mysqlclient is slow on PyPy
DBHOST = os.environ.get('DBHOST', 'localhost')


# setup

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = DBDRIVER + '://benchmarkdbuser:benchmarkdbpass@%s:3306/hello_world?charset=utf8' % DBHOST
app.config['JSONIFY_PRETTYPRINT_REGULAR'] = False
db = SQLAlchemy(app)
dbraw_engine = create_engine(app.config['SQLALCHEMY_DATABASE_URI'], connect_args={'autocommit': True}, pool_reset_on_return=None)

bakery = baked.bakery()


# models

class World(db.Model):
    __tablename__ = "World"
    id = db.Column(db.Integer, primary_key=True)
    randomNumber = db.Column(db.Integer)

    # http://stackoverflow.com/questions/7102754/jsonify-a-sqlalchemy-result-set-in-flask
    @property
    def serialize(self):
        """Return object data in easily serializeable format"""
        return {
            'id'         : self.id,
            'randomNumber': self.randomNumber
        }

    @staticmethod
    def get(ident):
        baked_query = bakery(lambda s: s.query(World))
        return baked_query(db.session()).get(ident)


class Fortune(db.Model):
    __tablename__ = "Fortune"
    id = db.Column(db.Integer, primary_key=True)
    message = db.Column(db.String)


# views

# flask.jsonify doesn't allow array at top level for security concern.
# So we should have oriiginal one.
def json_response(obj):
    res = make_response(json.dumps(obj))
    res.mimetype = "application/json"
    return res


@app.route("/json")
def hello():
    return jsonify(message='Hello, World!')


@app.route("/db")
def get_random_world():
    num_queries = request.args.get("queries", 1, type=int)
    if num_queries < 1:
        num_queries = 1
    if num_queries > 500:
        num_queries = 500
    worlds = [World.get(randint(1, 10000)).serialize
              for _ in xrange(num_queries)]
    return json_response(worlds)


@app.route("/dbs")
def get_random_world_single():
    wid = randint(1, 10000)
    worlds = World.get(wid).serialize
    return json_response(worlds)


@app.route("/dbraw")
def get_random_world_raw():
    connection = dbraw_engine.connect()
    num_queries = request.args.get("queries", 1, type=int)
    if num_queries < 1:
        num_queries = 1
    if num_queries > 500:
        num_queries = 500
    worlds = []
    for i in xrange(num_queries):
        wid = randint(1, 10000)
        result = connection.execute("SELECT * FROM World WHERE id = " + str(wid)).fetchone()
        worlds.append({'id': result[0], 'randomNumber': result[1]})
    connection.close()
    return json_response(worlds)


@app.route("/dbsraw")
def get_random_world_single_raw():
    connection = dbraw_engine.connect()
    wid = randint(1, 10000)
    result = connection.execute("SELECT * FROM World WHERE id = " + str(wid)).fetchone()
    worlds = {'id': result[0], 'randomNumber': result[1]}
    connection.close()
    return json_response(worlds)

@app.route("/fortunes")
def get_fortunes():
    fortunes = list(Fortune.query.all())
    fortunes.append(Fortune(id=0, message="Additional fortune added at request time."))
    fortunes.sort(key=attrgetter('message'))
    return render_template('fortunes.html', fortunes=fortunes)

@app.route("/fortunesraw")
def get_forutens_raw():
    res = dbraw_engine.execute("SELECT * FROM Fortune")
    fortunes = res.fetchall()
    res.close()
    fortunes.append(Fortune(id=0, message="Additional fortune added at request time."))
    fortunes.sort(key=attrgetter('message'))
    return render_template('fortunes.html', fortunes=fortunes)


@app.route("/updates")
def updates():
    """Test 5: Database Updates"""
    num_queries = request.args.get('queries', 1, type=int)
    if num_queries < 1:
        num_queries = 1
    if num_queries > 500:
        num_queries = 500

    worlds = []
    rp = partial(randint, 1, 10000)
    ids = [rp() for _ in xrange(num_queries)]
    ids.sort()  # To avoid deadlock
    for id in ids:
        world = World.get(id)
        world.randomNumber = rp()
        worlds.append(world.serialize)
    res = json_response(worlds)
    db.session.commit()
    return res


@app.route("/raw-updates")
def raw_updates():
    """Test 5: Database Updates"""
    connection = dbraw_engine.connect()
    try:
        num_queries = request.args.get('queries', 1, type=int)
        if num_queries < 1:
            num_queries = 1
        if num_queries > 500:
            num_queries = 500

        worlds = []
        rp = partial(randint, 1, 10000)
        for i in xrange(num_queries):
            world = connection.execute("SELECT * FROM World WHERE id=%s", (rp(),)).fetchone()
            randomNumber = rp()
            worlds.append({'id': world['id'], 'randomNumber': randomNumber})
            connection.execute("UPDATE World SET randomNumber=%s WHERE id=%s", (randomNumber, world['id']))
        return json_response(worlds)
    finally:
        connection.close()


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
