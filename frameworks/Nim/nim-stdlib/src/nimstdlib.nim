import asynchttpserver, asyncdispatch

import handlers

const port = 8080

proc main {.async.} =
    var server = newAsyncHttpServer()

    proc cb(req: Request) {.async.} =
        case req.reqMethod
        of HttpGet:
            case req.url.path
            of "/plaintext":    await handlePlaintext(req)
            of "/json":         await handleJson(req)
            else:               await handle404Error(req)
        else:                   await handle405Error(req)


    server.listen Port(port)
    echo "Starting server on port: " & $port
    while true:
        if server.shouldAcceptRequest():
            await server.acceptRequest(cb)
        else:
            poll()

asyncCheck main()
runForever()
