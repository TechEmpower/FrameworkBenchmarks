
# Hexagon Benchmarking Test

This is the Hexagon portion of a [benchmarking test suite](../../../README.md) comparing a variety
of web development platforms. The test utilizes Hexagon routes and serialization.

## Tests

You can verify the benchmarks with the following command (from the project root):
`./tfb --mode verify --test hexagon hexagon-jetty hexagon-tomcat hexagon-netty hexagon-nettyepoll`

To run the full benchmarks locally, on the project root (not this directory) execute:
`./tfb --mode benchmark --test hexagon hexagon-jetty hexagon-tomcat hexagon-netty hexagon-nettyepoll`

## Infrastructure Software Versions

* [Hexagon stable version](http://hexagonkt.com)

## Test URLs

### Jetty

* JSON Encoding Test: http://localhost:9090/json
* Plain Text Test: http://localhost:9090/plaintext
* Data-Store/Database Mapping Test: http://localhost:9090/db?queries=5
* Fortunes: http://localhost:9090/fortunes
* Database updates: http://localhost:9090/update
* Database queries: http://localhost:9090/query

### Netty

* JSON Encoding Test: http://localhost:9090/json
* Plain Text Test: http://localhost:9090/plaintext
* Data-Store/Database Mapping Test: http://localhost:9090/db?queries=5
* Fortunes: http://localhost:9090/fortunes
* Database updates: http://localhost:9090/update
* Database queries: http://localhost:9090/query

### Netty Epoll

* JSON Encoding Test: http://localhost:9090/json
* Plain Text Test: http://localhost:9090/plaintext
* Data-Store/Database Mapping Test: http://localhost:9090/db?queries=5
* Fortunes: http://localhost:9090/fortunes
* Database updates: http://localhost:9090/update
* Database queries: http://localhost:9090/query

### Tomcat

* JSON Encoding Test: http://localhost:8080/json
* Plain Text Test: http://localhost:8080/plaintext
* Data-Store/Database Mapping Test: http://localhost:8080/db?queries=5
* Fortunes: http://localhost:8080/fortunes
* Database updates: http://localhost:8080/update
* Database queries: http://localhost:8080/query
