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
            of "/queries":      await handleQueries(req)
            of "/fortunes":     await handleFortunes(req)
            of "/updates":      await handleUpdates(req)
            else:               await handleHTTPErrors(req, Http404, "URL doesn't exists")
        else:               await handleHTTPErrors(req, Http405, "Method not allowed")

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
