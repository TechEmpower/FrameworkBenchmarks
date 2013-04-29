from bottle import Bottle, route, request, run
from bottle.ext import sqlalchemy 
from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from random import randint

app = Bottle()
app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql://benchmarkdbuser:benchmarkdbpass@DBHOSTNAME:3306/hello_world'
Base = declarative_base()
db_engine = create_engine(app.config['SQLALCHEMY_DATABASE_URI'])
plugin = sqlalchemy.Plugin(db_engine, keyword='db', )
app.install(plugin)


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

@app.route("/json")
def hello():
  resp = {"message": "Hello, World!"}
  return jsonify(resp)

@app.route("/db")
def get_random_world(db):
  num_queries = request.query.queries or '1'
  worlds = []
  for i in range(int(num_queries)):
    wid = randint(1, 10000)
    worlds.append(World.query.get(wid).serialize)
  return jsonify(worlds=worlds)

@app.route("/dbs")
def get_random_world_single(db):
  wid = randint(1, 10000)
  worlds = [World.query.get(wid).serialize]
  return jsonify(worlds=worlds)
  
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
  return jsonify(worlds=worlds)

@app.route("/dbsraw")
def get_random_world_single_raw():
  connection = db_engine.connect()
  wid = randint(1, 10000)
  result = connection.execute("SELECT * FROM world WHERE id = " + str(wid)).fetchone()
  worlds = [{'id': result[0], 'randomNumber': result[1]}]
  connection.close()
  return jsonify(worlds=worlds)

if __name__ == "__main__":
    app.run()
