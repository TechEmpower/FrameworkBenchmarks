using Pkg

Pkg.activate(@__DIR__)

using HTTP
import JSON3
using Dates

@info "starting listener"
HTTP.listen("0.0.0.0", 60822, reuseaddr=true) do http
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
      JSON3.write(http, (; message="Hello, World!"))
   else
      HTTP.setstatus(http, 404)
      startwrite(http)
      write(http, "Not Found")
   end
end
