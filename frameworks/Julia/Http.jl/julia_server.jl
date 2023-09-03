using Pkg
using Dates
using HTTP
using LibPQ
using JSON3
using StructTypes
using ConcurrentUtilities: ConcurrentUtilities, Pools
using HypertextLiteral: @htl
using Logging

include("Pool.jl")
include("API.jl")

function notfound(request)
    @info "Not found"
    return HTTP.Response(404, [], "")
end

router = HTTP.Router()

HTTP.register!(router, "GET", "/plaintext", plaintext)
HTTP.register!(router, "GET", "/json", jsonSerialization)
HTTP.register!(router, "GET", "/db", singleQuery)
HTTP.register!(router, "GET", "/queries", multipleQueries)
HTTP.register!(router, "GET", "/updates", updates)
HTTP.register!(router, "GET", "/fortunes", fortunes)
HTTP.register!(router, "/**", notfound)
HTTP.register!(router, "/", notfound)

# running multiple threads doesn't seem to make any sense
# https://docs.julialang.org/en/v1/stdlib/Sockets/#Base.bind

@info "Julia runs on $(Threads.nthreads()) threads"

# Logging.disable_logging(Logging.Error)
preinit_pool()
HTTP.serve("0.0.0.0", 8080; backlog = -1, reuseaddr = true) do request::HTTP.Request
    return router(request)
end
