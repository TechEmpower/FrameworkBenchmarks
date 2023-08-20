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

# ConcurrentUtilities.init(40)
@info "Julia runs on $(Threads.nthreads()) threads"

function restart_server()
    server = HTTP.listen!("0.0.0.0", 8080; reuseaddr = true) do http
        # t = ConcurrentUtilities.@spawn handler(http)
        # wait(t)
        handler(http)
        return nothing
    end
end

for _ in Iterators.repeated(true)
    try
        server = restart_server()
        wait(server)
    catch exc
        try
            close(server)
        catch
        end
        if exc isa InterruptException
            rethrow()
        end
        @info "Server died, restarting" (exc, catch_backtrace())
    end
end
