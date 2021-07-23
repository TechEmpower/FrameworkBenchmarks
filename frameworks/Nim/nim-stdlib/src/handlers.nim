import asynchttpserver, asyncdispatch, times, random, db_postgres, cgi, strtabs, strutils, json, algorithm

import defs

proc handleHTTPErrors*(req: Request, code: HttpCode, msg: string) {.async.} =
    let headers = {"Date": now().utc.format("ddd, dd MMM yyyy HH:mm:ss") & " GMT", "Content-type": "text/plain; charset=utf-8"}
    await req.respond(code, msg, headers.newHttpHeaders())

proc handlePlaintext*(req: Request) {.async.} =
    let headers = {"Date": now().utc.format("ddd, dd MMM yyyy HH:mm:ss") & " GMT", "Content-type": "text/plain; charset=utf-8",  "Server": "Example"}
    await req.respond(Http200, "Hello, World!", headers.newHttpHeaders())

proc handleJson*(req: Request) {.async.} =
    let headers = {"Date": now().utc.format("ddd, dd MMM yyyy HH:mm:ss") & " GMT", "Content-type": "application/json; charset=utf-8", "Server": "Example"}
    await req.respond(Http200, """{"message":"Hello, World!"}""", headers.newHttpHeaders())

proc handleDB*(req: Request) {.async.} =
    let headers = {"Date": now().utc.format("ddd, dd MMM yyyy HH:mm:ss") & " GMT", "Content-type": "application/json; charset=utf-8", "Server": "Example"}
    let queryResult = db.getRow(sql"""select * from "public"."World" where id = ?""", rand(1..10000))
    await req.respond(Http200, """ {"id":""" & queryResult[0] & ""","randomNumber":""" & queryResult[1] & """}""", headers.newHttpHeaders())

proc handleQueries*(req: Request) {.async.} =
    let queryObj = readData(req.url.query)
    var count = 1
    try:
        count = clamp(parseInt(queryObj["queries"]), 1, 500)
    except KeyError, ValueError:
        count = 1
    except:
        await handleHTTPErrors(req, Http502, "Something is wrong")

    var jsonList: DBQueryObjsList
    for i in 1..count:
        let queryResult = db.getRow(sql"""select * from "public"."World" where id = ?""", rand(1..10000))
        let jsonNode = DBQueryObj(id: queryResult[0], randomNumber: queryResult[1])
        jsonList.add(jsonNode)

    let headers = {"Date": now().utc.format("ddd, dd MMM yyyy HH:mm:ss") & " GMT", "Content-type": "application/json; charset=utf-8", "Server": "Example"}
    await req.respond(Http200, $(%*jsonList), headers.newHttpHeaders())

proc handleFortunes*(req: Request) {.async.} =
    var queryResult = db.getAllRows(sql"""select * from "public"."fortune" """)
    queryResult.add(@["0",  "Additional fortune added at request time."])
    let sortedResult = queryResult.sortedByIt(it[1])

    var fortunesView = """
<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body>
<table>
<tr><th>id</th><th>message</th></tr>
"""

    for i, v in sortedResult:
        let fragment = "<tr><td>" & $v[0] & "</td><td>" & $v[1].xmlEncode() & "</td></tr>\n"
        fortunesView.add(fragment)

    fortunesView.add """
</table>
</body>
</html>
"""

    let headers = {"Date": now().utc.format("ddd, dd MMM yyyy HH:mm:ss") & " GMT", "Content-type": "text/html; charset=UTF-8", "Server": "Example"}
    await req.respond(Http200, fortunesView, headers.newHttpHeaders())
