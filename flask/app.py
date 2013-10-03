#!/usr/bin/env python
from functools import partial
from operator import attrgetter
from random import randint
import sys

import flask
from flask import Flask, jsonify, request, render_template
from flask.ext.sqlalchemy import SQLAlchemy
from sqlalchemy import create_engine

if sys.version_info[0] == 3:
    xrange = range

try:
    import MySQLdb
    mysql_schema = "mysql:"
except ImportError:
    mysql_schema = "mysql+pymysql:"

# setup

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = mysql_schema + '//benchmarkdbuser:benchmarkdbpass@localhost:3306/hello_world?charset=utf8'
app.config['JSONIFY_PRETTYPRINT_REGULAR'] = False
db = SQLAlchemy(app)
dbraw_engine = create_engine(app.config['SQLALCHEMY_DATABASE_URI'])

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

class Fortune(db.Model):
    __tablename__ = "Fortune"
    id = db.Column(db.Integer, primary_key=True)
    message = db.Column(db.String)


# views

@app.route("/json")
def hello():
    return jsonify(message='Hello, World!')


@app.route("/db")
def get_random_world():
    num_queries = request.args.get("queries", 1, type=int)
    worlds = [World.query.get(randint(1, 10000)).serialize
              for _ in xrange(num_queries)]
    return jsonify(worlds=worlds)


@app.route("/dbs")
def get_random_world_single():
    wid = randint(1, 10000)
    worlds = [World.query.get(wid).serialize]
    return jsonify(worlds=worlds)


@app.route("/dbraw")
def get_random_world_raw():
    connection = dbraw_engine.connect()
    num_queries = request.args.get("queries", 1, type=int)
    worlds = []
    for i in xrange(num_queries):
        wid = randint(1, 10000)
        result = connection.execute("SELECT * FROM World WHERE id = " + str(wid)).fetchone()
        worlds.append({'id': result[0], 'randomNumber': result[1]})
    connection.close()
    return jsonify(worlds=worlds)


@app.route("/dbsraw")
def get_random_world_single_raw():
    connection = dbraw_engine.connect()
    wid = randint(1, 10000)
    result = connection.execute("SELECT * FROM World WHERE id = " + str(wid)).fetchone()
    worlds = [{'id': result[0], 'randomNumber': result[1]}]
    connection.close()
    return jsonify(worlds=worlds)

@app.route("/fortunes")
def get_fortunes():
    fortunes = list(Fortune.query.all())
    fortunes.append(Fortune(id=0, message="Additional fortune added at request time."))
    fortunes.sort(key=attrgetter('message'))
    return render_template('fortunes.html', fortunes=fortunes)

@app.route("/fortunesraw")
def get_forutens_raw():
    fortunes = list(dbraw_engine.execute("SELECT * FROM Fortune"))
    fortunes.append(Fortune(id=0, message="Additional fortune added at request time."))
    fortunes.sort(key=attrgetter('message'))
    return render_template('fortunes.html', fortunes=fortunes)


@app.route("/updates")
def updates():
    """Test 5: Database Updates"""
    num_queries = request.args.get('queries', 1, type=int)
    if num_queries > 500:
        num_queries = 500

    worlds = []
    rp = partial(randint, 1, 10000)
    ids = [rp() for _ in xrange(num_queries)]
    ids.sort()  # To avoid deadlock
    for id in ids:
        world = World.query.get(id)
        world.randomNumber = rp()
        worlds.append(world.serialize)
    db.session.commit()
    return jsonify(worlds)


@app.route("/raw-updates")
def raw_updates():
    """Test 5: Database Updates"""
    num_queries = request.args.get('queries', 1, type=int)
    if num_queries > 500:
        num_queries = 500

    worlds = []
    rp = partial(randint, 1, 10000)
    for i in xrange(num_queries):
        world = dbraw_engine.execute("SELECT * FROM World WHERE id=%s", (rp(),)).fetchone()
        randomNumber = rp()
        worlds.append({'id': world['id'], 'randomNumber': randomNumber})
        dbraw_engine.execute("UPDATE World SET randomNumber=%s WHERE id=%s",
                             (randomNumber, world['id']))
    return jsonify(worlds=worlds)


@app.route('/plaintext')
def plaintext():
    """Test 6: Plaintext"""
    response = flask.make_response(b'Hello, World!')
    response.content_type = 'text/plain'
    return response


# entry point for debugging
if __name__ == "__main__":
    app.run(debug=True)
