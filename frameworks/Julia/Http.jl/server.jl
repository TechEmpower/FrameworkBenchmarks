using Pkg

Pkg.activate(@__DIR__)

using HTTP
import JSON3
using Dates
using Sockets

@info "starting listener"
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
      JSON3.write(http, (;message ="Hello, World!"))
   else
       HTTP.setstatus(http, 404)
       startwrite(http)
       write(http, "Not Found")
   end
end

# function getHeaders(contentType::String)
#    return [
#       "Server" => "Julia-HTTP",
#       "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT",
#       "Content-Type" => contentType
#    ]
# end
#
# # "service" functions to actually do the work
# function helloWorldPlain(req::HTTP.Request)
#    return HTTP.Response(200, getHeaders("text/plain"), bytes("Hello, World!"))
# end
#
# function helloWorldJSON(req::HTTP.Request)
#    res = HTTP.Response(JSON3.write(Dict(:message => "Hello, World!")))
#    res.headers = HTTP.mkheaders(getHeaders("application/json"))
#    return res
# end
#
# # define REST endpoints to dispatch to "service" functions
# const TBM_ROUTER = HTTP.Router()
# HTTP.@register(TBM_ROUTER, "GET", "/plaintext", helloWorldPlain)
# HTTP.@register(TBM_ROUTER, "GET", "/json", helloWorldJSON)
#
# @info "Starting server on port 8080"
# HTTP.serve(TBM_ROUTER, ip"0.0.0.0", 8080, reuseaddr=true, verbose=true)
