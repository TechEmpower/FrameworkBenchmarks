from flask import Flask, jsonify, request, render_template
from flask.ext.sqlalchemy import SQLAlchemy
from sqlalchemy import create_engine, select
from random import randint
from operator import attrgetter, itemgetter
from functools import partial

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql://benchmarkdbuser:benchmarkdbpass@DBHOSTNAME:3306/hello_world?charset=utf8'
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

class Fortune(db.Model):
  __tablename__ = "Fortune"
  id = db.Column(db.Integer, primary_key=True)
  message = db.Column(db.Unicode)

  # http://stackoverflow.com/questions/7102754/jsonify-a-sqlalchemy-result-set-in-flask
  @property
  def serialize(self):
     """Return object data in easily serializeable format"""
     return {
         'id'     : self.id,
         'message': self.message
     }

@app.route("/json")
def hello():
  resp = {"message": "Hello, World!"}
  return jsonify(resp)

@app.route("/db")
def get_random_world():
  num_queries = request.args.get("queries", 1)
  worlds = []
  rp = partial(randint, 10000)
  for i in xrange(int(num_queries)):
    worlds.append(World.query.get(rp()).serialize)
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
  rp = partial(randint, 10000)
  for i in xrange(int(num_queries)):
    result = connection.execute("SELECT * FROM world WHERE id = " + str(rp())).fetchone()
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

@app.route("/fortune")
def fortune_orm():
  fortunes=Fortune.query.all()
  fortunes.append(Fortune(message="Additional fortune added at request time."))
  fortunes=sorted(fortunes, key=attrgetter('message'))
  return render_template("fortune-obj.html", fortunes=fortunes)

@app.route("/fortuneraw")
def fortune_raw():
  connection = dbraw_engine.connect()
  fortunes=[(f.id, f.message) for f in connection.execute("SELECT * FROM Fortune")]
  fortunes.append((0L, u'Additional fortune added at request time.'))
  fortunes=sorted(fortunes, key=itemgetter(1))
  connection.close()
  return render_template("fortune.html", fortunes=fortunes)


if __name__ == "__main__":
    app.run(host='0.0.0.0', debug=True)
