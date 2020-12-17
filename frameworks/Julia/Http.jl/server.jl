using Pkg

Pkg.activate(@__DIR__)

using HTTP
import JSON3
using Dates
using Sockets

function getHeaders(contentType::String)
   return [
      "Server" => "Julia-HTTP",
      "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT",
      "Content-Type" => contentType
   ]
end

# "service" functions to actually do the work
function helloWorldPlain(req::HTTP.Request)
   res = HTTP.Response("Hello, World!")
   res.headers = HTTP.mkheaders(getHeaders("text/plain"))
   return res
end

function helloWorldJSON(req::HTTP.Request)
   res = HTTP.Response(JSON3.write((;message = "Hello, World!")))
   res.headers = HTTP.mkheaders(getHeaders("application/json"))
   return res
end

# define REST endpoints to dispatch to "service" functions
const TBM_ROUTER = HTTP.Router()
HTTP.@register(TBM_ROUTER, "GET", "/plaintext", helloWorldPlain)
HTTP.@register(TBM_ROUTER, "GET", "/json", helloWorldJSON)

@info "Starting server on port 8080"
HTTP.serve(TBM_ROUTER, ip"0.0.0.0", 8080, reuseaddr=true)
