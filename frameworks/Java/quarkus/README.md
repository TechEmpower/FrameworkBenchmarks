# Quarkus Benchmarking Test

This is the Quarkus portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Implementations

There are currently two repository implementations.

### JAX-RS and Hibernate via JPA
* [Plaintext test source](hibernate/src/main/java/io/quarkus/benchmark/resource/PlaintextResource.java)
* [JSON test source](hibernate/src/main/java/io/quarkus/benchmark/resource/JsonResource.java)
* [Query, Queries, Update test source](hibernate/src/main/java/io/quarkus/benchmark/repository/hibernate/WorldRepository.java)
* [Fortunes test source](hibernate/src/main/java/io/quarkus/benchmark/repository/hibernate/FortuneRepository.java)

### Reactive Routes and Asynchronous DB accesses via pgclient
* [Plaintext test source](pgclient/src/main/java/io/quarkus/benchmark/resource/pgclient/PlaintextResource.java)
* [JSON test source](pgclient/src/main/java/io/quarkus/benchmark/resource/pgclient/JsonResource.java)
* [Query, Queries, Update test source](pgclient/src/main/java/io/quarkus/benchmark/repository/pgclient/WorldRepository.java)
* [Fortunes test source](pgclient/src/main/java/io/quarkus/benchmark/repository/pgclient/FortuneRepository.java)

## Versions

* [Java OpenJDK 11](http://openjdk.java.net/)
* [Quarkus 1.1.1](https://quarkus.io)

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
