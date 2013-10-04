from bottle import Bottle, route, request, run, template, response
from bottle.ext import sqlalchemy 
from sqlalchemy import create_engine, Column, Integer, Unicode
from sqlalchemy.ext.declarative import declarative_base
from random import randint
import sys
from operator import attrgetter, itemgetter
from functools import partial

try:
    import ujson as json
except ImportError:
    import json

app = Bottle()
app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql://benchmarkdbuser:benchmarkdbpass@localhost:3306/hello_world?charset=utf8'
Base = declarative_base()
db_engine = create_engine(app.config['SQLALCHEMY_DATABASE_URI'])
plugin = sqlalchemy.Plugin(db_engine, keyword='db', )
app.install(plugin)

if sys.version_info[0] == 3:
    xrange = range


class World(Base):
  __tablename__ = "World"
  id = Column(Integer, primary_key=True)
  randomNumber = Column(Integer)

  # http://stackoverflow.com/questions/7102754/jsonify-a-sqlalchemy-result-set-in-flask
  @property
  def serialize(self):
     """Return object data in easily serializeable format"""
     return {
         'id'         : self.id,
         'randomNumber': self.randomNumber
     }

class Fortune(Base):
  __tablename__ = "Fortune"
  id = Column(Integer, primary_key=True)
  message = Column(Unicode)


@app.route("/json")
def hello():
    response.content_type = 'application/json'
    resp = {"message": "Hello, World!"}
    return json.dumps(resp)

@app.route("/db")
def get_random_world(db):
    num_queries = request.query.get('queries', 1, type=int)
    worlds = []
    rp = partial(randint, 1, 10000)
    for i in xrange(num_queries):
        worlds.append(db.query(World).get(rp()).serialize)
    response.content_type = 'application/json'
    return json.dumps(worlds)

@app.route("/dbs")
def get_random_world_single(db):
    wid = randint(1, 10000)
    worlds = [db.query(World).get(wid).serialize]
    response.content_type = 'application/json'
    return json.dumps(worlds)
  
@app.route("/dbraw")
def get_random_world_raw():
    connection = db_engine.connect()
    num_queries = request.query.get('queries', 1, type=int)
    worlds = []
    rp = partial(randint, 1, 10000)
    for i in xrange(int(num_queries)):
        result = connection.execute("SELECT * FROM world WHERE id = " + str(rp())).fetchone()
        worlds.append({'id': result[0], 'randomNumber': result[1]})
    connection.close()
    response.content_type = 'application/json'
    return json.dumps(worlds)

@app.route("/dbsraw")
def get_random_world_single_raw():
    connection = db_engine.connect()
    wid = randint(1, 10000)
    result = connection.execute("SELECT * FROM world WHERE id = " + str(wid)).fetchone()
    worlds = [{'id': result[0], 'randomNumber': result[1]}]
    connection.close()
    response.content_type = 'application/json'
    return json.dumps(worlds)

@app.route("/fortune")
def fortune_orm(db):
  fortunes=db.query(Fortune).all()
  fortunes.append(Fortune(message="Additional fortune added at request time."))
  fortunes=sorted(fortunes, key=attrgetter('message'))
  return template('fortune-obj', fortunes=fortunes)

@app.route("/fortuneraw")
def fortune_raw():
    connection = db_engine.connect()
    fortunes=[(f.id, f.message) for f in connection.execute("SELECT * FROM Fortune")]
    fortunes.append((0, u'Additional fortune added at request time.'))
    fortunes=sorted(fortunes, key=itemgetter(1))
    connection.close()
    return template('fortune', fortunes=fortunes)


@app.route("/updates")
def updates(db):
    """Test 5: Database Updates"""
    num_queries = request.query.get('queries', 1, type=int)
    if num_queries > 500:
        num_queries = 500

    worlds = []
    rp = partial(randint, 1, 10000)
    ids = [rp() for _ in xrange(num_queries)]
    ids.sort()  # To avoid deadlock
    for id in ids:
        world = db.query(World).get(id)
        world.randomNumber = rp()
        worlds.append(world.serialize)

    response.content_type = 'application/json'
    return json.dumps(worlds)


@app.route("/raw-updates")
def raw_updates():
    """Test 5: Database Updates"""
    num_queries = request.query.get('queries', 1, type=int)
    if num_queries > 500:
        num_queries = 500

    conn = db_engine.connect()

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
    response.content_type = 'text/plain'
    return b'Hello, World!'


if __name__ == "__main__":
    app.run(host='0.0.0.0', debug=False)
