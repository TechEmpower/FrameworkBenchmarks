using Pkg
Pkg.activate(@__DIR__)

using Dates
using HTTP
using MySQL
using JSON3

HTTP.listen("0.0.0.0" , 8080, reuseaddr = true) do http
    target = http.message.target

    if endswith(target, "/plaintext")
        HTTP.setstatus(http, 200)
        HTTP.setheader(http, "Content-Type" => "text/plain")
        HTTP.setheader(http, "Server" => "Julia-HTTP")
        HTTP.setheader(http, "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT")

        HTTP.startwrite(http)
        write(http, "Hello, World!")

    elseif endswith(target, "/json")
        HTTP.setheader(http, "Content-Type" => "application/json")
        HTTP.setheader(http, "Server" => "Julia-HTTP")
        HTTP.setheader(http, "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT")
        HTTP.setstatus(http, 200)

        startwrite(http)
        JSON3.write(http, (;message = "Hello, World!"))

    elseif endswith(target, "/db")
        randNum = rand(1:10000)

        HTTP.setheader(http, "Content-Type" => "application/json")
        HTTP.setheader(http, "Server" => "Julia-HTTP")
        HTTP.setheader(http, "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT")
        HTTP.setstatus(http, 200)

        conn = DBInterface.connect(MySQL.Connection, "tfb-database", "benchmarkdbuser", "benchmarkdbpass", db="hello_world")
        sqlQuery = "SELECT * FROM World WHERE id = $randNum"
        results = DBInterface.execute(conn, sqlQuery)
        row = first(results)
        dbNumber = row[2]
        jsonString = "{\"id\":$randNum,\"randomNumber\":$dbNumber}"

        startwrite(http)
        JSON3.write(http, (JSON3.read(jsonString)))

    elseif occursin("/queries", target)
        HTTP.setheader(http, "Content-Type" => "application/json")
        HTTP.setheader(http, "Server" => "Julia-HTTP")
        HTTP.setheader(http, "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT")
        HTTP.setstatus(http, 200)

        try
            numQueries = parse(Int64, (split(target, "="))[2])

        catch ArgumentError
            numQueries = -1
            
        finally
            if numQueries == -1 or occursin("foo", numQueries)
                HTTP.setstatus(http, 404)
                startwrite(http)
                write(http, "Not Found")

        randNumList = rand(Int64, 1:10000, numQueries)
        conn = DBInterface.connect(MySQL.Connection, "tfb-database", "benchmarkdbuser", "benchmarkdbpass", db="hello_world")

        responseArray = "["
        for i in 1:numQueries
            randNum = randNumList[i]
            sqlQuery = "SELECT * FROM World WHERE id = $randNum"
            results = DBInterface.execute(conn, sqlQuery)
            row = first(results)
            dbNumber = row[2]
            responseArray = reponseArray * "{\"id\":$randNum,\"randomNumber\":$dbNumber}"

            if i < numQueries
                reponseArray = responseArray * ","
            end
        end
        reponseArray = responseArray * "]"

        startwrite(http)
        JSON3.write(http, (JSON3.read(responseArray)))

    else
        HTTP.setstatus(http, 404)
        startwrite(http)
        write(http, "Not Found")

    end
end
