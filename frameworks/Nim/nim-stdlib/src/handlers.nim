import asynchttpserver, asyncdispatch, times

proc handle404Error*(req: Request) {.async.} =
    let headers = {"Date": now().utc.format("ddd, dd MMM yyyy HH:mm:ss") & " GMT", "Content-type": "text/plain; charset=utf-8"}
    await req.respond(Http404, "URL doesn't exists", headers.newHttpHeaders())

proc handle405Error*(req: Request) {.async.} =
    let headers = {"Date": now().utc.format("ddd, dd MMM yyyy HH:mm:ss") & " GMT", "Content-type": "text/plain; charset=utf-8"}
    await req.respond(Http405, "Method not allowed", headers.newHttpHeaders())

proc handlePlaintext*(req: Request) {.async.} =
    let headers = {"Date": now().utc.format("ddd, dd MMM yyyy HH:mm:ss") & " GMT", "Content-type": "text/plain; charset=utf-8"}
    await req.respond(Http200, 'Hello, World!', headers.newHttpHeaders())

proc handleJson*(req: Request) {.async.} =
    let headers = {"Date": now().utc.format("ddd, dd MMM yyyy HH:mm:ss") & " GMT", "Content-type": "application/json; charset=utf-8", "Server": "Example"}
    await req.respond(Http200, """{"message":"Hello, World!"}""", headers.newHttpHeaders())
