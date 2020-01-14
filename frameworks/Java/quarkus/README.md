# Quarkus Benchmarking Test

This is the Quarkus portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

There is currently one repository implementation.
* [WorldRepository](src/main/java/io/quarkus/benchmark/repository/WorldRepository.java) is using JPA.

### Plaintext Test

* [Plaintext test source](src/main/java/io/quarkus/benchmark/resource/PlainTextResource.java)

### JSON Serialization Test

* [JSON test source](src/main/java/io/quarkus/benchmark/resource/JsonResource.java)

### Database Query Test

* [Database Query test source](src/main/java/io/quarkus/benchmark/resource/DbResource.java)

### Database Queries Test

* [Database Queries test source](src/main/java/io/quarkus/benchmark/resource/DbResource.java)

### Database Update Test

* [Database Update test source](src/main/java/io/quarkus/benchmark/resource/DbResource.java)

### Template rendering Test

* [Template rendering test source](src/main/java/io/quarkus/benchmark/resource/FortuneResource.java)

## Versions

* [Java OpenJDK 11](http://openjdk.java.net/)
* [Quarkus 0.16.0](https://quarkus.io)

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
