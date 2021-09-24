using Pkg
Pkg.activate(@__DIR__)

using Dates
using HTTP
using MySQL
using JSON3

HTTP.listen("0.0.0.0" , 8080, reuseaddr = true) do http
    if endswith(http.message.target, "/plaintext")
        HTTP.setstatus(http, 200)
        HTTP.setheader(http, "Content-Type" => "text/plain")
        HTTP.setheader(http, "Server" => "Julia-HTTP")
        HTTP.setheader(http, "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT")

        HTTP.startwrite(http)
        write(http, "Hello, World!")

    elseif endswith(http.message.target, "/json")
        HTTP.setheader(http, "Content-Type" => "application/json")
        HTTP.setheader(http, "Server" => "Julia-HTTP")
        HTTP.setheader(http, "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT")
        HTTP.setstatus(http, 200)

        startwrite(http)
        JSON3.write(http, (;message = "Hello, World!"))

    elseif endswith(http.message.target, "/db")
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

    else
        HTTP.setstatus(http, 404)
        startwrite(http)
        write(http, "Not Found")
    end
end
