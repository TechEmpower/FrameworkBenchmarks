# Genie Benchmarking Test

Project structure made with `Genie.Generator.newapp_webservice("GenieBenchmark")`

## How to run
 * Update database configuration in config/env/dev.jl
 * Use bin/server to startup the server

### Implemented benchmarks
- [x] JSON serialization
- [] Single query
- [] Multiple queries
- [x] Plaintext
- [] Fortunes
- [] Updates

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=

### CACHED QUERY

http://localhost:8080/cached_query?queries=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes
