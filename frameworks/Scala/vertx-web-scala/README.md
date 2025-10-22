# Vert.x Web Scala Benchmarking Test

This is the Vert.x Web Scala portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### Test Type Implementation Source Code

* [JSON](src/main/scala/vertx/App.scala)
* [PLAINTEXT](src/main/scala/vertx/App.scala)
* [DB](src/main/scala/vertx/App.scala)
* [QUERY](src/main/scala/vertx/App.scala)
* [UPDATE](src/main/scala/vertx/App.scala)
* [FORTUNES](src/main/scala/vertx/App.scala)

## Versions

* [Java OpenJDK 17](https://openjdk.java.net/)
* [Vert.x 3.9](https://vertx.io/)

## Test URLs

### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes
