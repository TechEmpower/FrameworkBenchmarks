import asynchttpserver, asyncdispatch, db_postgres

import defs, handlers

const port = 8080

proc main {.async.} =
    var server = newAsyncHttpServer()

    proc cb(req: Request) {.async.} =
        case req.reqMethod
        of HttpGet:
            case req.url.path
            of "/plaintext":    await handlePlaintext(req)
            of "/json":         await handleJson(req)
            of "/db":           await handleDB(req)
            else:               await handle404Error(req)
        else:                   await handle405Error(req)

    server.listen Port(port)
    echo "Starting server on port: " & $port
    
    let dbPing = db.getValue(sql"select now()")
    echo "Connected to Database at: " & $dbPing
    
    while true:
        if server.shouldAcceptRequest():
            await server.acceptRequest(cb)
        else:
            poll()

asyncCheck main()
runForever()
