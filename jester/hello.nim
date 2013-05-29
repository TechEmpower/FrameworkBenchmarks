import jester, strtabs, json, asyncio, sockets

get "/json":
  var obj = %{"message": %"Hello, World!"}
  resp($obj)

var disp = newDispatcher()
disp.register(port = TPort(9000), http=false)
while disp.poll(): nil