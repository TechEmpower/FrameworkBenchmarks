
struct jsonMsgObj
    message::String
end

struct jsonObj
    id::Int64
    randomNumber::Int64
end

StructTypes.StructType(::Type{jsonMsgObj}) = StructTypes.Struct()
StructTypes.StructType(::Type{jsonObj}) = StructTypes.Struct()

function getqueries(req)
    params = HTTP.queryparams(HTTP.URI(req.target).query)
    return try
        min(500, max(1, parse(Int64, get(params, "queries", "1"))))
    catch ArgumentError
        1
    end
end

function plaintext(req::HTTP.Request)
    headers = [
        "Content-Type" => "text/plain",
        "Server" => "Julia-HTTP",
        "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT",
    ]

    return HTTP.Response(200, headers, body = "Hello, World!")
end

function jsonSerialization(req::HTTP.Request)
    headers = [
        "Content-Type" => "application/json",
        "Server" => "Julia-HTTP",
        "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT",
    ]

    return HTTP.Response(200, headers, body = JSON3.write(jsonMsgObj("Hello, World!")))
end

function singleQuery(req::HTTP.Request)
    headers = [
        "Content-Type" => "application/json",
        "Server" => "Julia-HTTP",
        "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT",
    ]

    randNum = rand(1:10000)

    dbNumber = withdb() do conn
        sqlQuery = "SELECT randomnumber, id FROM world WHERE id = \$1"
        results = LibPQ.execute(conn, sqlQuery, [randNum])
        first(first(results))
    end

    return HTTP.Response(200, headers, body = JSON3.write(jsonObj(randNum, dbNumber)))
end

function multipleQueries(req::HTTP.Request)
    headers = [
        "Content-Type" => "application/json",
        "Server" => "Julia-HTTP",
        "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT",
    ]

    nqueries = getqueries(req)
    responseArray = sizehint!(Vector{jsonObj}(), nqueries)
    for i = 1:nqueries
        withdb() do conn
            randNum = rand(1:10000)
            results = LibPQ.execute(conn, "SELECT * FROM World WHERE id = \$1 ", [randNum])
            push!(responseArray, jsonObj(randNum, first(results)[2]))
        end
    end
    return HTTP.Response(200, headers, body = JSON3.write(responseArray))
end

function updates(req::HTTP.Request)
    headers = [
        "Content-Type" => "application/json",
        "Server" => "Julia-HTTP",
        "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT",
    ]

    nqueries = getqueries(req)
    responseArray = sizehint!(Vector{jsonObj}(), nqueries)
    withdb() do conn
        for i = 1:nqueries
            randId = rand(1:10000)
            randNum = rand(1:10000)
            sqlQuery = "SELECT * FROM World WHERE id = $randId"
            results = LibPQ.execute(conn, sqlQuery)
            row = first(results)
            dbNumber = row[2]
            sqlQuery = "UPDATE World SET randomnumber = $randNum WHERE id = $randId"
            results = LibPQ.execute(conn, sqlQuery)
            push!(responseArray, jsonObj(randId, randNum))
        end
    end

    return HTTP.Response(200, headers, body = JSON3.write(responseArray))
end

function fortunes(req::HTTP.Request)
    headers = [
        "Content-Type" => "text/html; charset=utf-8",
        "Server" => "Julia-HTTP",
        "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT",
    ]

    sqlQuery = "SELECT * FROM fortune"
    output = ""
    results = withdb() do conn
        LibPQ.execute(conn, sqlQuery)
    end
    fortunesList = [[string(row[1]), row[2]] for row in results]
    push!(fortunesList, [string(0), "Additional fortune added at request time."])
    sort!(fortunesList, by = x -> x[2])

    body = "<!DOCTYPE html>" * string(@htl("""
    <html>
        <head>
            <title>Fortunes</title>
        </head>
        <body>
            <table>
                <tr><th>id</th><th>message</th></tr>
                $([@htl("<tr><td>$(f[1])</td><td>$(f[2])</td></tr>\n") for f in fortunesList])
            </table>
        </body>
    </html>
    """))
    return HTTP.Response(200, headers, body)
end
