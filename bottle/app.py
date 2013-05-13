from bottle import Bottle, route, request, run, template
from bottle.ext import sqlalchemy 
from sqlalchemy import create_engine, Column, Integer, Unicode
from sqlalchemy.ext.declarative import declarative_base
from random import randint
import ujson
from operator import attrgetter, itemgetter
from functools import partial

app = Bottle()
app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql://benchmarkdbuser:benchmarkdbpass@DBHOSTNAME:3306/hello_world?charset=utf8'
Base = declarative_base()
db_engine = create_engine(app.config['SQLALCHEMY_DATABASE_URI'])
plugin = sqlalchemy.Plugin(db_engine, keyword='db', )
app.install(plugin)


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
  resp = {"message": "Hello, World!"}
  return ujson.dumps(resp)

@app.route("/db")
def get_random_world(db):
  num_queries = request.query.queries or '1'
  worlds = []
  rp = partial(randint, 1, 10000)
  for i in xrange(int(num_queries)):
    worlds.append(db.query(World).get(rp()).serialize)
  return ujson.dumps(worlds)

@app.route("/dbs")
def get_random_world_single(db):
  wid = randint(1, 10000)
  worlds = [db.query(World).get(wid).serialize]
  return ujson.dumps(worlds)
  
@app.route("/dbraw")
def get_random_world_raw():
  connection = db_engine.connect()
  num_queries = request.query.queries or '1'
  worlds = []
  rp = partial(randint, 1, 10000)
  for i in range(int(num_queries)):
    result = connection.execute("SELECT * FROM world WHERE id = " + str(rp())).fetchone()
    worlds.append({'id': result[0], 'randomNumber': result[1]})
  connection.close()
  return ujson.dumps(worlds)

@app.route("/dbsraw")
def get_random_world_single_raw():
  connection = db_engine.connect()
  wid = randint(1, 10000)
  result = connection.execute("SELECT * FROM world WHERE id = " + str(wid)).fetchone()
  worlds = [{'id': result[0], 'randomNumber': result[1]}]
  connection.close()
  return ujson.dumps(worlds)

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
  fortunes.append((0L, u'Additional fortune added at request time.'))
  fortunes=sorted(fortunes, key=itemgetter(1))
  connection.close()
  return template('fortune', fortunes=fortunes)

if __name__ == "__main__":
    app.run(host='0.0.0.0', debug=True)
