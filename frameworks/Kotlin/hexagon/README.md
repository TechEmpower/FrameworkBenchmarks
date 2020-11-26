
# Hexagon Benchmarking Test

This is the Hexagon portion of a [benchmarking test suite](../../../README.md) comparing a variety
of web development platforms. The test utilizes Hexagon routes, serialization and database access.

## Tests

You can run tests against any running server passing the `verify.endpoint` project property. I.e.:
`./gradlew verify -Pverify.endpoint=http://host:1234`

* [Hexagon Web](src/main/kotlin/com/hexagonkt/Benchmark.kt)
* [Hexagon Storage](src/main/kotlin/com/hexagonkt/BenchmarkStorage.kt)

## Infrastructure Software Versions

* [Hexagon stable version](http://hexagonkt.com)

## Test URLs

In URLs replace `${DB_ENGINE}` with one of:

* mongodb
* postgresql

and `${TEMPLATE_ENGINE}` with: `pebble`

### Jetty

* JSON Encoding Test: http://localhost:9090/json
* Plain Text Test: http://localhost:9090/plaintext
* Data-Store/Database Mapping Test: http://localhost:9090/${DB_ENGINE}/db?queries=5
* Fortunes: http://localhost:9090/${DB_ENGINE}/${TEMPLATE_ENGINE}/fortunes
* Database updates: http://localhost:9090/${DB_ENGINE}/update
* Database queries: http://localhost:9090/${DB_ENGINE}/query

### Resin

* JSON Encoding Test: http://localhost:8080/json
* Plain Text Test: http://localhost:8080/plaintext
* Data-Store/Database Mapping Test: http://localhost:8080/${DB_ENGINE}/db?queries=5
* Fortunes: http://localhost:8080/${DB_ENGINE}/${TEMPLATE_ENGINE}/fortunes
* Database updates: http://localhost:8080/${DB_ENGINE}/update
* Database queries: http://localhost:8080/${DB_ENGINE}/query
