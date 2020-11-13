using HTTP
using JSON
using Dates

HTTP.listen("0.0.0.0", 8080, reuseaddr=true) do http
   HTTP.setheader(http, "Server" => "Julia-HTTP")
   HTTP.setheader(http, "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT")
   if endswith(http.message.target, "/plaintext")
      HTTP.setheader(http, "Content-Type" => "text/plain")
      HTTP.setstatus(http, 200)
      startwrite(http)
      write(http, "Hello, World!")
   elseif endswith(http.message.target, "/json")
      HTTP.setheader(http, "Content-Type" => "application/json")
      HTTP.setstatus(http, 200)
      startwrite(http)
      write(http, JSON.json(Dict(:message => "Hello, World!")))
   else
       HTTP.setstatus(http, 404)
       startwrite(http)
       write(http, "Not Found")
   end
end
