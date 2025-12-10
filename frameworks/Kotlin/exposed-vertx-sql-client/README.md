# Exposed Vert.x SQL Client Benchmarking Test

Adapted from the [`vertx-web-kotlinx` portion](../vertx-web-kotlinx) for the [Exposed Vert.x SQL Client](https://github.com/huanshankeji/exposed-vertx-sql-client) library.

### Test Type Implementation Source Code

* [DB](src/main/kotlin/MainVerticle.kt)
* [QUERY](src/main/kotlin/MainVerticle.kt)
* [UPDATE](src/main/kotlin/MainVerticle.kt)
* [FORTUNES](src/main/kotlin/MainVerticle.kt)

## Important Libraries
The tests were run with:
* [Those in the `vertx-web-kotlinx` portion](../vertx-web-kotlinx/README.md#important-libraries)
* [Exposed Vert.x SQL Client](https://github.com/huanshankeji/exposed-vertx-sql-client)

## Test URLs
### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes
