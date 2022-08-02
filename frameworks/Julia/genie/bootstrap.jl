(pwd() != @__DIR__) && cd(@__DIR__) # allow starting app from bin/ dir

using GenieBenchmark
const UserApp = GenieBenchmark
GenieBenchmark.main()
