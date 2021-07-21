import asynchttpserver, asyncdispatch, times

import src.handlers

proc main {.async.} =
    var server = newAsyncHttpServer()
    

    proc cb(req: Request) {.async.} =
        let headers = {"Date": now().utc.format("ddd, dd MMM yyyy HH:mm:ss") & " GMT", "Content-type": "text/plain; charset=utf-8"}            
        case req.reqMethod
        of HttpGet:
            case req.url.path
            of "/plaintext":    await handlePlaintext(req)
            of "/json":         await handleJson(req)
            else:
                await req.respond(Http404, "404 Error", headers.newHttpHeaders())

        of HttpPost:
            case req.url.path
            of "/posty":
                await req.respond(Http200, "POsted something", headers.newHttpHeaders())
            else:
                await req.respond(Http404, "404 Error", headers.newHttpHeaders())

        else:
            await req.respond(Http405, "Method not allowed", headers.newHttpHeaders())



    
    server.listen Port(8080)
    while true:
        if server.shouldAcceptRequest():
            await server.acceptRequest(cb)
        else:
            poll()

asyncCheck main()
runForever()
