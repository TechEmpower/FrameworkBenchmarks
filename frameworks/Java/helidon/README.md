# Helidon Benchmarking Test

This is the Helidon portion of a [benchmarking test suite](../) comparing a variety of web development platforms.
Two Helidon APIs are implemented: Reactive and Nima. The Reactive API has been around since the first
version of Helidon while Nima is a new, blocking API based on JDK 20 virtual threads. 

The code is organized into two Maven modules, namely, *reactive* and *nima*. Both modules implement the
same set of tests outlined below. 

### Plaintext Test

* [Reactive](src/main/java/io/helidon/benchmark/reactive/services/PlainTextService.java)
* [Nima](src/main/java/io/helidon/benchmark/nima/Main.java)

### JSON Serialization Test

* [Reactive](src/main/java/io/helidon/benchmark/reactive/services/JsonService.java)
* [Nima](src/main/java/io/helidon/benchmark/nima/Main.java)

### Database Query Test

* [Reactive](src/main/java/io/helidon/benchmark/reactive/services/DbService.java)
* [Nima](src/main/java/io/helidon/benchmark/nima/services/DbService.java)

### Database Queries Test

* [Reactive](src/main/java/io/helidon/benchmark/reactive/services/DbService.java)
* [Nima](src/main/java/io/helidon/benchmark/nima/services/DbService.java)

### Database Update Test

* [Reactive](src/main/java/io/helidon/benchmark/reactive/services/DbService.java)
* [Nima](src/main/java/io/helidon/benchmark/nima/services/DbService.java)

### Template Rendering Test

* [Reactive](src/main/java/io/helidon/benchmark/reactive/services/FortuneService.java)
* [Nima](src/main/java/io/helidon/benchmark/nima/services/FortuneHandler.java)

## Versions

* Reactive
  * [Java OpenJDK 11](http://openjdk.java.net/)
  * [Helidon 3.0.1](http://helidon.io/)
  
* Nima
  * [Java OpenJDK 19 w/Loom](http://openjdk.java.net/)
  * [Helidon 4.0.0-ALPHA1](http://helidon.io/)

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
