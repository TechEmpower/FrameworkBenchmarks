#!/usr/bin/env python
from flask import Flask, jsonify, request, render_template
from flask.ext.sqlalchemy import SQLAlchemy
from sqlalchemy import create_engine
from random import randint
from operator import attrgetter

try:
    import MySQLdb
    mysql_schema = "mysql:"
except ImportError:
    mysql_schema = "mysql+pymysql:"

# setup

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = mysql_schema + '//benchmarkdbuser:benchmarkdbpass@DBHOSTNAME:3306/hello_world?charset=utf8'
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

# entry point for debugging
if __name__ == "__main__":
    app.run(debug=True)
