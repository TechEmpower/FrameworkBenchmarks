# Vertx Benchmarking Test

This is the Vert.x portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### Plaintext Test

* [Plaintext test source](src/main/java/vertx/App.java)

### JSON Serialization Test

* [JSON test source](src/main/java/vertx/App.java)

### Database Query Test

* [Database Query test source](src/main/java/vertx/App.java)

### Database Queries Test

* [Database Queries test source](src/main/java/vertx/App.java)

### Database Update Test

* [Database Update test source](src/main/java/vertx/App.java)

### Template rendering Test

* [Template rendering test source](src/main/java/vertx/App.java)

### Caching Test

* [Caching test source](src/main/java/vertx/App.java)

## Versions

* [Java 17](https://jdk.java.net)
* [vertx 4.3.8](http://vertx.io/)

## Test URLs

### Plaintext Test

    http://localhost:8080/plaintext

### JSON Encoding Test

    http://localhost:8080/json

### Database Query Test

    http://localhost:8080/db

### Database Queries Test

    http://localhost:8080/queries?queries=5

### Database Update Test

    http://localhost:8080/updates?queries=5

### Template rendering Test

    http://localhost:8080/fortunes

### Cached Query Test

    http://localhost:8080/cached-queries?count=10
