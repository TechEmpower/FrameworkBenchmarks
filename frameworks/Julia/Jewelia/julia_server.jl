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
    HTTP.setheader(http, "Content-Type" => "text/plain")
    HTTP.setheader(http, "Server" => "Julia-HTTP")
    HTTP.setheader(http, "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT")

    if endswith(target, "/plaintext")
        HTTP.startwrite(http)
        write(http, "Hello, World!")

    elseif endswith(target, "/json")
        startwrite(http)
        JSON3.write(http, (;message = "Hello, World!"))

    elseif endswith(target, "/db")
        randNum = rand(1:10000)

        conn = DBInterface.connect(MySQL.Connection, "tfb-database", "benchmarkdbuser", "benchmarkdbpass", db="hello_world")
        sqlQuery = "SELECT * FROM World WHERE id = $randNum"
        results = DBInterface.execute(conn, sqlQuery)
        row = first(results)
        dbNumber = row[2]
        jsonString = "{\"id\":$randNum,\"randomNumber\":$dbNumber}"

        startwrite(http)
        JSON3.write(http, (JSON3.read(jsonString)))

    elseif occursin("/queries", target)
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

        # randNumList = rand(Int64, 1:10000, numQueries)
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

    else
        HTTP.setstatus(http, 404)
        startwrite(http)
        write(http, "Not Found")

    end
end
