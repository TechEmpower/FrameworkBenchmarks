import json, random
from strutils import parseInt
# framework
import basolato/controller
import allographer/query_builder


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

proc queries*(this:BenchmarkController):Response =
  try:
    var countNum = this.request.params["queries"].parseInt()
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
  except:
    raise newException(Error500, "")


proc index*(this:BenchmarkController):Response =
  return render("index")

proc show*(this:BenchmarkController, id:string):Response =
  let id = id.parseInt
  return render("show")

proc create*(this:BenchmarkController):Response =
  return render("create")

proc store*(this:BenchmarkController):Response =
  return render("store")

proc edit*(this:BenchmarkController, id:string):Response =
  let id = id.parseInt
  return render("edit")

proc update*(this:BenchmarkController, id:string):Response =
  let id = id.parseInt
  return render("update")

proc destroy*(this:BenchmarkController, id:string):Response =
  let id = id.parseInt
  return render("destroy")
