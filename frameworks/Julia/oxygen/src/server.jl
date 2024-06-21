
using Oxygen
using Dates
using HTTP

@get "/json" function()
   return json(("message" => "Hello, World!"))
end

@get "/plaintext" function()
   return text("Hello, World!")
end

function HeaderMiddleware(handle::Function)
   function(req::HTTP.Request)
      response = handle(req)
      HTTP.setheader(response, "Server" => "Julia-Oxygen")
      HTTP.setheader(response, "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT")
      return response
   end
end

serveparallel(host="0.0.0.0", port=8080, middleware=[HeaderMiddleware], access_log=nothing, metrics=false, docs=false)