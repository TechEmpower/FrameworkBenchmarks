
# Hexagon Benchmarking Test

This is the Hexagon portion of a [benchmarking test suite](../) comparing a variety of web
development platforms. The test utilizes Hexagon routes, serialization and database access.

## Tests

* [Hexagon Web](/src/main/java/com/hexagonkt/Benchmark.kt)
* [Hexagon Storage](/src/main/java/com/hexagonkt/BenchmarkStorage.kt)

## Infrastructure Software Versions

* [Hexagon stable version](http://hexagonkt.com)

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

## Run inside vagrant

    rm -rf ~/FrameworkBenchmarks/results
    ~/FrameworkBenchmarks/toolset/run-tests.py --mode verify --test hexagon
    ~/FrameworkBenchmarks/toolset/run-tests.py --mode verify --test hexagon-resin
