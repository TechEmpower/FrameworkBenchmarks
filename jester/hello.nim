import jester, strtabs, json, asyncio, sockets, os, strutils

get "/json":
  var obj = %{"message": %"Hello, World!"}
  resp($obj, "application/json")

var disp = newDispatcher()
disp.register(port = TPort(paramStr(1).parseInt), http=false)
while disp.poll(): nil