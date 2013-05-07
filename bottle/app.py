from bottle import Bottle, route, request, run
from bottle.ext import sqlalchemy 
from sqlalchemy import create_engine, Column, Integer
from sqlalchemy.ext.declarative import declarative_base
from random import randint
import ujson

app = Bottle()
app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql://benchmarkdbuser:benchmarkdbpass@DBHOSTNAME:3306/hello_world'
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

@app.route("/json")
def hello():
  resp = {"message": "Hello, World!"}
  return ujson.dumps(resp)

@app.route("/db")
def get_random_world(db):
  num_queries = request.query.queries or '1'
  worlds = []
  for i in range(int(num_queries)):
    wid = randint(1, 10000)
    worlds.append(db.query(World).get(wid).serialize)
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
  for i in range(int(num_queries)):
    wid = randint(1, 10000)
    result = connection.execute("SELECT * FROM world WHERE id = " + str(wid)).fetchone()
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

@app.route("/update")
def update_worlds(db):
  num_queries = int(request.query.queries or '1')
  if num_queries < 1:
    num_queries = 1
  else if num_queries > 500:
    num_queries = 500

  worlds = []
  for i in range(num_queries):
    wid = randint(1, 10000)
    world = db.query(World).get(wid)
    world.randomNumber = randint(1, 10000)
    db.commit()
    worlds.append(world.serialize)
  return ujson.dumps(worlds)

if __name__ == "__main__":
    app.run()
