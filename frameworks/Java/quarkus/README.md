# Quarkus Benchmarking Test

This is the Quarkus portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Implementations
There is currently two repository implementations.

### Hibernate via JPA
* [WorldRepository](hibernate/src/main/java/io/quarkus/benchmark/repository/hibernate/WorldRepository.java)
* [FortuneRepository](hibernate/src/main/java/io/quarkus/benchmark/repository/hibernate/FortuneRepository.java)

### Asynchronous DB accesses via pgclient
* [WorldRepository](pgclient/src/main/java/io/quarkus/benchmark/repository/pgclient/WorldRepository.java)
* [FortuneRepository](pgclient/src/main/java/io/quarkus/benchmark/repository/pgclient/FortuneRepository.java)


### Plaintext Test

* [Plaintext test source](base/src/main/java/io/quarkus/benchmark/resource/PlaintextResource.java)

### JSON Serialization Test

* [JSON test source](base/src/main/java/io/quarkus/benchmark/resource/JsonResource.java)

### Database Query Test

* [Hibernate Database Query test source](hibernate/src/main/java/io/quarkus/benchmark/resource/hibernate/DbResource.java)
* [PGClient Database Query test source](pgclient/src/main/java/io/quarkus/benchmark/resource/pgclient/DbResource.java)

### Database Queries Test

* [Hibernate Database Query test source](hibernate/src/main/java/io/quarkus/benchmark/resource/hibernate/DbResource.java)
* [PGClient Database Query test source](pgclient/src/main/java/io/quarkus/benchmark/resource/pgclient/DbResource.java)

### Database Update Test

* [Hibernate Database Query test source](hibernate/src/main/java/io/quarkus/benchmark/resource/hibernate/DbResource.java)
* [PGClient Database Query test source](pgclient/src/main/java/io/quarkus/benchmark/resource/pgclient/DbResource.java)

### Template rendering Test

* [Hibernate Template rendering test source](hibernate/src/main/java/io/quarkus/benchmark/resource/hibernate/FortuneResource.java)
* [PGClient Template rendering test source](pgclient/src/main/java/io/quarkus/benchmark/resource/pgclient/FortuneResource.java)

## Versions

* [Java OpenJDK 11](http://openjdk.java.net/)
* [Quarkus 1.1.1](https://quarkus.io)

## Test URLs

### Plaintext Test

    http://localhost:8080/plaintext

### JSON Encoding Test

    http://localhost:8080/json

### Database Query Test

    http://localhost:8080/hibernate/db
    http://localhost:8080/pgclient/db

### Database Queries Test

    http://localhost:8080/hibernate/queries?queries=5
    http://localhost:8080/pgclient/queries?queries=5

### Database Update Test

    http://localhost:8080/hibernate/updates?queries=5
    http://localhost:8080/pgclient/updates?queries=5

### Template rendering Test

    http://localhost:8080/hibernate/fortunes
    http://localhost:8080/pgclient/fortunes
