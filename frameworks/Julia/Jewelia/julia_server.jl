using Pkg
Pkg.activate(@__DIR__)

using Dates
using HTTP
using MySQL
using JSON3
using StructTypes

struct jsonObj
    id::Int
    randomNumber::Int
end

StructTypes.StructType(::Type{jsonObj}) = StructTypes.Struct()

    function plaintext(req::HTTP.Request)
        headers = [ "Content-Type" => "text/plain", 
                    "Server" => "Julia-HTTP",
                    "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT" ]
    
        return HTTP.Response(200, headers, body = "Hello, World!")
    end
    
    function jsonSerialization(req::HTTP.Request)
        headers = [ "Content-Type" => "application/json",
                    "Server" => "Julia-HTTP",
                    "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT" ]
 
        return HTTP.Response(200, headers, body = JSON3.write(JSON3.read('{\'Message\' : \'Hello, World!\'}')))
    end
        
    function singleQuery(req::HTTP.Request)
        headers = [ "Content-Type" => "application/json",
                    "Server" => "Julia-HTTP",
                    "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT" ]
    
        randNum = rand(1:10000)

        conn = DBInterface.connect(MySQL.Connection, "tfb-database", "benchmarkdbuser", "benchmarkdbpass", db="hello_world")
        sqlQuery = "SELECT * FROM World WHERE id = $randNum"
        results = DBInterface.execute(conn, sqlQuery)
        row = first(results)
        dbNumber = row[2]
        jsonString = "{\"id\":$randNum,\"randomNumber\":$dbNumber}"
        
        DBInterface.close!(conn)
        return HTTP.Response(200, headers, body = JSON3.write((JSON3.read(jsonString))))
    end
        
    function multipleQueries(req::HTTP.Request)
        headers = [ "Content-Type" => "application/json",
                    "Server" => "Julia-HTTP",
                    "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT" ]
        
        numQueries = -1

        try
            numQueries = parse(Int64, (split(req.target, "="))[2])

        catch ArgumentError
            numQueries = 1

        finally
            if numQueries > 500
                numQueries = 500
            end

            if numQueries < 1
                numQueries = 1
            end
        end

        conn = DBInterface.connect(MySQL.Connection, "tfb-database", "benchmarkdbuser", "benchmarkdbpass", db="hello_world")

        responseArray = Array{jsonObj}(undef, numQueries)
        for i in 1:numQueries
            # randNum = randNumList[i]
            randNum = rand(1:10000)
            sqlQuery = "SELECT * FROM World WHERE id = $randNum"
            results = DBInterface.execute(conn, sqlQuery)
            row = first(results)
            dbNumber = row[2]
            responseArray[i] = JSON3.read("{\"id\":$randNum,\"randomNumber\":$dbNumber}", jsonObj)
        end
        
        DBInterface.close!(conn)
        
        return HTTP.Response(200, headers, body = JSON3.write(responseArray))
    end
        
    function updates(req::HTTP.Request)
        headers = [ "Content-Type" => "application/json",
                    "Server" => "Julia-HTTP",
                    "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT" ]
            
        numQueries = -1

        try
            numQueries = parse(Int64, (split(req.target, "="))[2])

        catch ArgumentError
            numQueries = 1

        finally
            if numQueries > 500
                numQueries = 500
            end

            if numQueries < 1
                numQueries = 1
            end
        end

        conn = DBInterface.connect(MySQL.Connection, "tfb-database", "benchmarkdbuser", "benchmarkdbpass", db="hello_world")

        responseArray = Array{jsonObj}(undef, numQueries)
        for i in 1:numQueries
            randId = rand(1:10000)
            randNum = rand(1:10000)
            sqlQuery = "SELECT * FROM World WHERE id = $randId"
            results = DBInterface.execute(conn, sqlQuery)
            row = first(results)
            dbNumber = row[2]

            sqlQuery = "UPDATE World SET randomnumber = $randNum WHERE id = $randId"
            results = DBInterface.execute(conn, sqlQuery)
            responseArray[i] = JSON3.read("{\"id\":$randId,\"randomNumber\":$randNum}", jsonObj)
        end
        
        DBInterface.close!(conn)
        return HTTP.Response(200, headers, body = JSON3.write(responseArray))
    end
        
    function fortunes(req::HTTP.Request)
        headers = [ "Content-Type" => "text/html; charset=utf-8",
                    "Server" => "Julia-HTTP",
                    "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT" ]
    
        fortunesList = []
        sqlQuery = "SELECT * FROM fortune"
        output = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"
        conn = DBInterface.connect(MySQL.Connection, "tfb-database", "benchmarkdbuser", "benchmarkdbpass", db="hello_world")
        results = DBInterface.execute(conn, sqlQuery)
    
        for row in results
            push!(fortunesList, [string(row[1]), row[2]])
        end
    
        push!(fortunesList, [string(0), "Additional fortune added at request time."])
    
        sort!(fortunesList, by = x -> x[2])
    
        for fortune in fortunesList
            id = fortune[1]
            message = HTTP.Strings.escapehtml(fortune[2])
            output = string(output, "<tr><td>$id</td><td>$message</td></tr>")
        end
    
        output = string(output, "</table></body></html>")
        
        DBInterface.close!(conn)
        return HTTP.Response(200, headers, body = output)
    end
            
const ROUTER = HTTP.Router()
HTTP.@register(ROUTER, "GET", "/plaintext", plaintext)
HTTP.@register(ROUTER, "GET", "/json", jsonSerialization)
HTTP.@register(ROUTER, "GET", "/db", singleQuery)
HTTP.@register(ROUTER, "GET", "/queries", multipleQueries)
HTTP.@register(ROUTER, "GET", "/updates", updates)
HTTP.@register(ROUTER, "GET", "/fortunes", fortunes)

HTTP.serve(ROUTER, "0.0.0.0" , 8080, reuseaddr=true)
