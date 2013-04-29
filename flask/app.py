from flask import Flask, jsonify, request
from flask.ext.sqlalchemy import SQLAlchemy
from sqlalchemy import create_engine
from random import randint

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql://benchmarkdbuser:benchmarkdbpass@DBHOSTNAME:3306/hello_world'
db = SQLAlchemy(app)
dbraw_engine = create_engine(app.config['SQLALCHEMY_DATABASE_URI'])

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
def get_random_world():
  num_queries = request.args.get("queries", 1)
  worlds = []
  for i in range(int(num_queries)):
    wid = randint(1, 10000)
    worlds.append(World.query.get(wid).serialize)
  return jsonify(worlds=worlds)

@app.route("/dbs")
def get_random_world_single():
  wid = randint(1, 10000)
  worlds = [World.query.get(wid).serialize]
  return jsonify(worlds=worlds)
  
@app.route("/dbraw")
def get_random_world_raw():
  connection = dbraw_engine.connect()
  num_queries = request.args.get("queries", 1)
  worlds = []
  for i in range(int(num_queries)):
    wid = randint(1, 10000)
    result = connection.execute("SELECT * FROM world WHERE id = " + str(wid)).fetchone()
    worlds.append({'id': result[0], 'randomNumber': result[1]})
  connection.close()
  return jsonify(worlds=worlds)

@app.route("/dbsraw")
def get_random_world_single_raw():
  connection = dbraw_engine.connect()
  wid = randint(1, 10000)
  result = connection.execute("SELECT * FROM world WHERE id = " + str(wid)).fetchone()
  worlds = [{'id': result[0], 'randomNumber': result[1]}]
  connection.close()
  return jsonify(worlds=worlds)

if __name__ == "__main__":
    app.run()
