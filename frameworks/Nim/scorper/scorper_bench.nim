import std / [macros, exitprocs]
import scorper
import jsony

type AsyncCallback = proc (request: Request): Future[void] {.closure, gcsafe,
    raises: [].}

type Resp = object
  message: string

proc jsonHandler(req: Request) {.route("get", "/json"), async.} =
  let headers = {"Content-type": "application/json"}
  await req.resp(Resp(message: "Hello, World!").toJson(), headers.newHttpHeaders())

proc plaintextHandler(req: Request) {.route("get", "/plaintext"), async.} =
  let headers = {"Content-type": "text/plain"}
  await req.resp("Hello, World!", headers.newHttpHeaders())

when isMainModule:

  let address = "0.0.0.0:8080"
  let flags = {ReuseAddr}
  let r = newRouter[AsyncCallback]()
  r.addRoute(jsonHandler)
  r.addRoute(plaintextHandler)
  var server = newScorper(address, r, flags)
  exitprocs.addExitProc proc() = server.stop();waitFor server.closeWait()
  server.start()

  waitFor server.join()
