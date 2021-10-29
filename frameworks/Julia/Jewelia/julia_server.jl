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

HTTP.listen("0.0.0.0" , 8080, reuseaddr = true) do http
    target = http.message.target

    HTTP.setstatus(http, 200)
    HTTP.setheader(http, "Server" => "Julia-HTTP")
    HTTP.setheader(http, "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT")

    if endswith(target, "/plaintext")
        HTTP.setheader(http, "Content-Type" => "text/plain")
        HTTP.startwrite(http)
        write(http, "Hello, World!")

    elseif endswith(target, "/json")
        HTTP.setheader(http, "Content-Type" => "application/json")
        startwrite(http)
        JSON3.write(http, (;message = "Hello, World!"))

    elseif endswith(target, "/db")
        HTTP.setheader(http, "Content-Type" => "application/json")
        randNum = rand(1:10000)

        conn = DBInterface.connect(MySQL.Connection, "tfb-database", "benchmarkdbuser", "benchmarkdbpass", db="hello_world")
        sqlQuery = "SELECT * FROM World WHERE id = $randNum"
        results = DBInterface.execute(conn, sqlQuery)
        row = first(results)
        dbNumber = row[2]
        jsonString = "{\"id\":$randNum,\"randomNumber\":$dbNumber}"

        startwrite(http)
        JSON3.write(http, (JSON3.read(jsonString)))
        
        DBInterface.close!(conn)
        
    elseif occursin("/queries", target)
        HTTP.setheader(http, "Content-Type" => "application/json")
        numQueries = -1

        try
            numQueries = parse(Int64, (split(target, "="))[2])

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

        startwrite(http)
        JSON3.write(http, responseArray)
        
        DBInterface.close!(conn)
    
    elseif occursin("/updates", target)
        HTTP.setheader(http, "Content-Type" => "application/json")
        numQueries = -1

        try
            numQueries = parse(Int64, (split(target, "="))[2])

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

        startwrite(http)
        JSON3.write(http, responseArray)
        
        DBInterface.close!(conn)

    elseif endswith(target, "/fortunes")
        HTTP.setheader(http, "Content-Type" => "text/html; charset=utf-8")
    
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
    
        write(http, output)
        
        DBInterface.close!(conn)
        
    else
        HTTP.setstatus(http, 404)
        startwrite(http)
        write(http, "Not Found")

    end
end
