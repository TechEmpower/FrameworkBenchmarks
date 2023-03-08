# Vert.x-Web Kotlinx Benchmarking Test

Vert.x-Web in Kotlin with request handling implemented as much with official kotlinx libraries as possible.

Code is written from scratch to be as concise as possible with common code extracted into common (possibly inline) functions. SQL client implementation details and JVM Options are adapted referring to [the vertx-web portion](../../Java/vertx-web) and [the vertx portion](../../Java/vertx). All requests are handled in coroutines and suspend `await`s are used instead of future compositions. Compared to [the vertx-web-kotlin-coroutines portion](../vertx-web-kotlin-coroutines), besides adopting the Kotlinx libraries, this project simplifies the code by using more built-in Coroutine functions and avoids mutability as much as possible. JSON serialization is implemented with kotlinx.serialization and Fortunes with kotlinx.html. The benchmark is run on the latest LTS version of JVM, 17.

## Test Type Implementation Source Code

* [JSON](src/main/kotlin/MainVerticle.kt)

  implemented with kotlinx.serialization

* [PLAINTEXT](src/main/kotlin/MainVerticle.kt)
* [DB](src/main/kotlin/MainVerticle.kt)
* [QUERY](src/main/kotlin/MainVerticle.kt)
* [CACHED QUERY](src/main/kotlin/MainVerticle.kt)
* [UPDATE](src/main/kotlin/MainVerticle.kt)
* [FORTUNES](src/main/kotlin/MainVerticle.kt)

  implemented with kotlinx.html

## Important Libraries

The tests were run with:

* [Vert.x-Web](https://vertx.io/docs/vertx-web/java/)
* [Vert.x Reactive PostgreSQL Client](https://vertx.io/docs/vertx-pg-client/java/)
* [kotlinx.coroutines](https://github.com/Kotlin/kotlinx.coroutines)
* [kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
* [kotlinx.html](https://github.com/Kotlin/kotlinx.html)

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
