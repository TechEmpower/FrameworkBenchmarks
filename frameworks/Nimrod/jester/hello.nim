import jester, strtabs, json, asyncdispatch, os, strutils

let port = Port(paramStr(1).parseInt)
let settings = newSettings(port=port, http=true)

routes:
  get "/json":
    var obj = %{"message": %"Hello, World!"}
    resp($obj, "application/json")

runForever()
