import random, json, strutils

import jester
import db_postgres

settings:
  port = Port(8080)

randomize()
let db = open("tfb-database", "benchmarkdbuser", "benchmarkdbpass", "hello_world")

type
  World = object
    id: int
    randomnumber: string

routes:
  get "/json":
    var data = $(%*{"message": "Hello, World!"})
    resp data, "application/json"

  get "/plaintext":
    const data = "Hello, World!"
    resp data, "text/plain"

  get "/db":
    var world_data: seq[JsonNode]
    
    let world = db.getRow(sql"SELECT * FROM world WHERE id = ?", rand(100))
    var row_data = %*World(id: parseInt(world[0]), randomnumber: world[1])
    world_data.add(row_data)

    resp $(%*world_data), "application/json"
    
  get "/queries":
    var world_datas: seq[JsonNode]
    var total = parseInt($request.params["queries"])

    var rand_data: seq[int]
    for idx in countup(1, total):
      rand_data.add(rand(900))

    for idx in rand_data:
      let world = db.getRow(sql"SELECT id, randomNumber FROM world WHERE id = ?", idx)
      var row_data = %*World(id: parseInt(world[0]), randomnumber: world[1])
      world_datas.add(row_data) 

    resp $(%*world_datas), "application/json"
    
  get "/fortunes":
    var fortunes_data: seq[JsonNode]

    for fortune in db.fastRows(sql"SELECT * FROM fortune"):
      let row_data = %*Fortune(id: parseInt(fortune[0]), message: fortune[1])
      fortunes_data.add(row_data)
      
    resp $(%*fortunes_data), "application/json"
    
