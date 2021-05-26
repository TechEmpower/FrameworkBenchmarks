import std / [macros, exitprocs]
import scorper


type AsyncCallback = proc (request: Request): Future[void] {.closure, gcsafe,
    raises: [].}

proc jsonHandler(req: Request) {.route("get", "/json"), async.} =
  let headers = {"Content-type": "application/json"}
  await req.resp("""{"message":"Hello, World!"}""", headers.newHttpHeaders())

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
  exitprocs.addExitProc proc() = waitFor server.closeWait()
  server.start()

  waitFor server.join()
