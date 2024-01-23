from functools import partial
from operator import attrgetter, itemgetter
from random import randint
from email.utils import formatdate
import os
import sys

from bottle import Bottle, route, request, run, template, response
from bottle.ext import sqlalchemy 
from sqlalchemy import create_engine, Column, Integer, Unicode
from sqlalchemy.ext.declarative import declarative_base

try:
    import ujson as json
except ImportError:
    import json

if sys.version_info[0] == 3:
    xrange = range

_is_pypy = hasattr(sys, 'pypy_version_info')

DBDRIVER = 'mysql+pymysql' if _is_pypy else 'mysql'
DBHOSTNAME = 'tfb-database'
DATABASE_URI = '%s://benchmarkdbuser:benchmarkdbpass@%s:3306/hello_world?charset=utf8' % (DBDRIVER, DBHOSTNAME)

app = Bottle()
Base = declarative_base()
db_engine = create_engine(DATABASE_URI)
plugin = sqlalchemy.Plugin(db_engine, keyword='db')
app.install(plugin)

# Engine for raw operation. Use autocommit.
raw_engine = create_engine(DATABASE_URI,
                           connect_args={'autocommit': True},
                           pool_reset_on_return=None)


class World(Base):
  __tablename__ = "World"
  id = Column(Integer, primary_key=True)
  randomNumber = Column(Integer)

  def serialize(self):
     """Return object data in easily serializeable format"""
     return {
         'id': self.id,
         'randomNumber': self.randomNumber,
     }


class Fortune(Base):
  __tablename__ = "Fortune"
  id = Column(Integer, primary_key=True)
  message = Column(Unicode)


@app.route("/json")
def hello():
    response.headers['Date'] = formatdate(timeval=None, localtime=False, usegmt=True)
    response.content_type = 'application/json'
    resp = {"message": "Hello, World!"}
    return json.dumps(resp)


@app.route("/db")
def get_random_world_single(db):
    """Test Type 2: Single Database Query"""
    response.headers['Date'] = formatdate(timeval=None, localtime=False, usegmt=True)
    wid = randint(1, 10000)
    world = db.query(World).get(wid).serialize()
    response.content_type = 'application/json'
    return json.dumps(world)


@app.route("/raw-db")
def get_random_world_single_raw():
    response.headers['Date'] = formatdate(timeval=None, localtime=False, usegmt=True)
    connection = raw_engine.connect()
    wid = randint(1, 10000)
    try:
        result = connection.execute("SELECT id, randomNumber FROM world WHERE id = " + str(wid)).fetchone()
        world = {'id': result[0], 'randomNumber': result[1]}
        response.content_type = 'application/json'
        return json.dumps(world)
    finally:
        connection.close()


@app.route("/queries")
def get_random_world(db):
    """Test Type 3: Multiple database queries"""
    response.headers['Date'] = formatdate(timeval=None, localtime=False, usegmt=True)
    num_queries = request.query.get('queries', 1, type=int)
    if num_queries < 1:
        num_queries = 1
    if num_queries > 500:
        num_queries = 500
    rp = partial(randint, 1, 10000)
    get = db.query(World).get
    worlds = [get(rp()).serialize() for _ in xrange(num_queries)]
    response.content_type = 'application/json'
    return json.dumps(worlds)


@app.route("/raw-queries")
def get_random_world_raw():
    response.headers['Date'] = formatdate(timeval=None, localtime=False, usegmt=True)
    num_queries = request.query.get('queries', 1, type=int)
    if num_queries < 1:
        num_queries = 1
    if num_queries > 500:
        num_queries = 500
    worlds = []
    rp = partial(randint, 1, 10000)
    connection = raw_engine.connect()
    try:
        for i in xrange(num_queries):
            result = connection.execute("SELECT id, randomNumber FROM world WHERE id = " + str(rp())).fetchone()
            worlds.append({'id': result[0], 'randomNumber': result[1]})
    finally:
        connection.close()
    response.content_type = 'application/json'
    return json.dumps(worlds)


@app.route("/fortunes")
def fortune_orm(db):
  response.headers['Date'] = formatdate(timeval=None, localtime=False, usegmt=True)
  fortunes=db.query(Fortune).all()
  fortunes.append(Fortune(id=0, message="Additional fortune added at request time."))
  fortunes.sort(key=attrgetter('message'))
  return template('fortune-obj', fortunes=fortunes)


@app.route("/raw-fortune")
def fortune_raw():
    response.headers['Date'] = formatdate(timeval=None, localtime=False, usegmt=True)
    connection = raw_engine.connect()
    try:
        fortunes=[(f.id, f.message) for f in connection.execute("SELECT * FROM Fortune")]
        fortunes.append((0, u'Additional fortune added at request time.'))
        fortunes=sorted(fortunes, key=itemgetter(1))
    finally:
        connection.close()
    return template('fortune', fortunes=fortunes)


@app.route("/updates")
def updates(db):
    """Test 5: Database Updates"""
    response.headers['Date'] = formatdate(timeval=None, localtime=False, usegmt=True)
    num_queries = request.query.get('queries', 1, type=int)
    if num_queries < 1:
        num_queries = 1
    if num_queries > 500:
        num_queries = 500

    worlds = []
    rp = partial(randint, 1, 10000)
    ids = [rp() for _ in xrange(num_queries)]
    ids.sort()  # To avoid deadlock
    for id in ids:
        world = db.query(World).get(id)
        world.randomNumber = rp()
        worlds.append(world.serialize())

    response.content_type = 'application/json'
    return json.dumps(worlds)


@app.route("/raw-updates")
def raw_updates():
    """Test 5: Database Updates"""
    response.headers['Date'] = formatdate(timeval=None, localtime=False, usegmt=True)
    num_queries = request.query.get('queries', 1, type=int)
    if num_queries < 1:
        num_queries = 1
    if num_queries > 500:
        num_queries = 500

    conn = raw_engine.connect()

    worlds = []
    rp = partial(randint, 1, 10000)
    for i in xrange(num_queries):
        world = conn.execute("SELECT * FROM World WHERE id=%s", (rp(),)).fetchone()
        randomNumber = rp()
        worlds.append({'id': world['id'], 'randomNumber': randomNumber})
        conn.execute("UPDATE World SET randomNumber=%s WHERE id=%s",
                     (randomNumber, world['id']))
    conn.close()
    response.content_type = 'application/json'
    return json.dumps(worlds)


@app.route('/plaintext')
def plaintext():
    """Test 6: Plaintext"""
    response.headers['Date'] = formatdate(timeval=None, localtime=False, usegmt=True)
    response.content_type = 'text/plain'
    return b'Hello, World!'


if __name__ == "__main__":
    app.run(host='0.0.0.0', debug=False)
