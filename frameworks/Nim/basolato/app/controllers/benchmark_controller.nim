import json, random, algorithm, cgi
from strutils import parseInt
# framework
import basolato/controller
import allographer/query_builder
# view
import ../../resources/pages/fortune_view

type BenchmarkController* = ref object of Controller

proc newBenchmarkController*(request:Request):BenchmarkController =
  randomize()
  return BenchmarkController.newController(request)


proc json*(this:BenchmarkController):Response =
  return render(%*{"message":"Hello, World!"})

proc plainText*(this:BenchmarkController):Response =
  var headers = newHeaders()
  headers.set("Content-Type", "text/plain; charset=UTF-8")
  return render("Hello, World!").setHeader(headers)

proc db*(this:BenchmarkController):Response =
  let i = rand(1..10000)
  let response = RDB().table("world").find(i)
  return render(%*response)

proc query*(this:BenchmarkController):Response =
  var countNum:int
  try:
    countNum = this.request.params["queries"].parseInt()
  except:
    countNum = 1

  if countNum < 1:
    countNum = 1
  elif countNum > 500:
    countNum = 500

  var response = newJArray()
  for _ in 1..countNum:
    let i = rand(1..10000)
    let data = RDB().table("world").find(i)
    response.add(data)
  return render(%*response)

proc fortune*(this:BenchmarkController):Response =
  type Data = ref object
    id:int
    message:string
  var tmp = newSeq[Data]()
  for row in RDB().table("Fortune").orderBy("message", Asc).get():
    tmp.add(Data(
      id: row["id"].getInt,
      message: xmlEncode(row["message"].getStr)
    ))
  tmp.add(Data(id: 0, message: "Additional fortune added at request time."))
  tmp = tmp.sortedByIt(it.message)
  var data = newSeq[JsonNode]()
  for row in tmp:
    data.add(%*{"id": row.id, "message": row.message})
  return render(this.view.fortuneView(data))

proc update*(this:BenchmarkController):Response =
  var countNum:int
  try:
    countNum = this.request.params["queries"].parseInt()
  except:
    countNum = 1

  if countNum < 1:
    countNum = 1
  elif countNum > 500:
    countNum = 500

  var response = newJArray()
  transaction:
    for _ in 1..countNum:
        let i = rand(1..10000)
        let newRandomNumber = rand(1..10000)
        discard RDB().table("world").find(i)
        RDB().table("world").where("id", "=", i).update(%*{"randomNumber": newRandomNumber})
        response.add(%*{"id":i, "randomNumber": newRandomNumber})
  return render(response)
