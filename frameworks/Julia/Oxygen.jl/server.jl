using Pkg

Pkg.activate(@__DIR__)

using Oxygen
using HTTP
import JSON3
using Dates

@info "starting listener"

@get "/json" function (req::HTTP.Request)
   return Dict("message" => "Hello, World!")
end

@get "/plaintext" function (req::HTTP.Request)
   return "Hello, World!"
end

function headerMod(handle)
   function (req)
      try
         response = handle(req)
         return HTTP.Response(200, ["Server" => "Julia-HTTP", "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT"], body=text(response))
      catch error
         @error "ERROR: " exception = (error, catch_backtrace())
         return HTTP.Response(500, "The Server encountered a problem")
      end
   end
end


serveparallel(middleware=[headerMod], access_log=nothing, port=8080)