using Oxygen
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


@get "/plaintext" plaintext
@get "/json" jsonSerialization
@get "/db" singleQuery
@get "/queries" multipleQueries
@get "/updates" updates
@get "/fortunes" fortunes

serveparallel(queuesize = 2 << 14, access_log = nothing, host = "0.0.0.0", port = 8080)
