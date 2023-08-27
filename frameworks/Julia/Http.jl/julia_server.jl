using Pkg
using Dates
using HTTP
using LibPQ
using JSON3
using StructTypes
using ConcurrentUtilities: ConcurrentUtilities, Pools
using HypertextLiteral: @htl

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

handler = HTTP.streamhandler(router)

# running multiple processes doesn't seem to make any sense
# https://docs.julialang.org/en/v1/stdlib/Sockets/#Base.bind


@info "Julia runs on $(Threads.nthreads()) threads"

backlog = try
    parse(Int, split(readchomp(`sysctl net.core.somaxconn`), " = ")[2])
catch
    511
end

HTTP.listen("0.0.0.0", 8080; backlog = backlog, reuseaddr = true) do http
    try
        handler(http)
    finally
        return nothing
    end
end
