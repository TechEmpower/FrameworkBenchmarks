# Loveqq MVC Benchmarking Test

This is the Loveqq MVC portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

An embedded reactor-netty is used for the web server.

### Plaintext Test

* [Plaintext test source](src/main/java/com/kfyty/benchmark/example/controller/WebMvcController.java)

### JSON Serialization Test

* [JSON test source](src/main/java/com/kfyty/benchmark/example/controller/WebMvcController.java)

### Database Query Test

* [Database Query test source](src/main/java/com/kfyty/benchmark/example/controller/WebMvcController.java)

### Database Queries Test

* [Database Queries test source](src/main/java/com/kfyty/benchmark/example/controller/WebMvcController.java)

### Database Update Test

* [Database Update test source](src/main/java/com/kfyty/benchmark/example/controller/WebMvcController.java)

### Template rendering Test

* [Template rendering test source](src/main/java/com/kfyty/benchmark/example/controller/WebMvcController.java)

## Versions

* [Java OpenJDK 21](http://openjdk.java.net/)
* [loveqq-framework 1.1.6-M5](http://github.com/kfyty/loveqq-framework)

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
