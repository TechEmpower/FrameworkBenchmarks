lapis = require "lapis"
db = require "lapis.db"
import Model from require "lapis.db.model"
import config from require "lapis.config"
import insert from table
import sort from table
import random from math

class Fortune extends Model

class World extends Model

lapis.serve class Benchmark extends lapis.Application
  "/": =>
    json: {message: "Hello, World!"}

  "/db": =>
    num_queries = tonumber(@params.queries) or 1
    worlds = {}
    for i = 1, num_queries
      insert worlds, World\find random(1, 10000)
    json: worlds

  "/fortunes": =>
    @fortunes = Fortune\select ""
    insert @fortunes, {id:0, message:"Additional fortune added at request time."}
    sort @fortunes, (a, b) -> a.message < b.message

    @title = "Fortunes"
    @html ->
      element "table", ->
        tr ->
          th ->
            text "id"
          th ->
            text "message"
        for fortune in *@fortunes
          tr ->
            td ->
              text fortune.id
            td ->
              text fortune.message

  "/update": =>
    num_queries = tonumber(@params.queries) or 1
    worlds = {}
    for i = 1, num_queries
      wid = random(1, 10000)
      world = World\find wid
      world.randomnumber = random(1, 10000)
      world\update "randomnumber"
      insert worlds, world
    json: worlds

  "/plaintext": =>
    content_type:"text/plain", layout: false, "Hello, World!"
              
