from flask import Flask, jsonify, request
from flask.ext.sqlalchemy import SQLAlchemy
from random import randint

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql://benchmarkdbuser:benchmarkdbpass@DBHOSTNAME:3306/hello_world'
db = SQLAlchemy(app)

class World(db.Model):
  __tablename__ = "World"
  id = db.Column(db.Integer, primary_key=True)
  randomNumber = db.Column(db.Integer)

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
    worlds.append(World.query.get(wid))
  return jsonify(worlds=worlds)
  
if __name__ == "__main__":
    app.run()
