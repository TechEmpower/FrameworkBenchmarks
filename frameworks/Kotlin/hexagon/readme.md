
# Hexagon Benchmarking Test

This is the Hexagon portion of a [benchmarking test suite](../) comparing a variety of web
development platforms. The test utilizes Hexagon routes, serialization and database access.

## Tests

* [Hexagon Web](/src/main/java/co/there4/hexagon/Benchmark.kt)
* [Hexagon Storage](/src/main/java/co/there4/hexagon/BenchmarkStorage.kt)

## Infrastructure Software Versions

* [Hexagon 0.10.x](http://there4.co/hexagon)

## Test URLs

### Jetty

* JSON Encoding Test: http://localhost:9090/json
* Data-Store/Database Mapping Test: http://localhost:9090/db?queries=5 
* Plain Text Test: http://localhost:9090/plaintext 
* Fortunes: http://localhost:9090/fortunes
* Database updates: http://localhost:9090/update
* Database queries: http://localhost:9090/query

### Resin

* JSON Encoding Test: http://localhost:8080/json
* Data-Store/Database Mapping Test: http://localhost:8080/db?queries=5 
* Plain Text Test: http://localhost:8080/plaintext 
* Fortunes: http://localhost:8080/fortunes
* Database updates: http://localhost:8080/update
* Database queries: http://localhost:8080/query
    
#### Resin configuration

    "resin" : { 
      "json_url" : "/json",
      "db_url" : "/db",
      "query_url" : "/query?queries=",
      "fortune_url" : "/fortunes",
      "update_url" : "/update?queries=",
      "plaintext_url" : "/plaintext",
 
      "port" : 8080,
 
      "approach" : "Realistic",
      "classification" : "Micro",
      "database" : "MongoDB",
      "framework" : "Hexagon",
      "language" : "Kotlin",
      "orm" : "Raw",
      "platform" : "Servlet",
      "webserver" : "Resin",
      "os" : "Linux",
      "database_os" : "Linux",
      "display_name" : "Hexagon Resin MongoDB",
      "notes" : "http://there4.co/hexagon",
 
      "setup_file" : "setup_resin",
      "versus" : "servlet"
    }                                   

## Run inside vagrant

    rm -rf ~/FrameworkBenchmarks/results
    ~/FrameworkBenchmarks/toolset/run-tests.py --mode verify --test hexagon
    ~/FrameworkBenchmarks/toolset/run-tests.py --mode verify --test hexagon-resin
