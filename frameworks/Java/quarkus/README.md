# Quarkus Benchmarking Test

This is the Quarkus portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Implementations

There are currently 6 repository implementations:

- RESTEasy and Hibernate ORM
- RESTEasy Reactive and Hibernate ORM
- RESTEasy Reactive and Hibernate Reactive
- RESTEasy Reactive and Vert.x PG Client
- Reactive Routes and Hibernate Reactive
- Reactive Routes and Vert.x PG Client

## Versions

* [Java OpenJDK 11](http://openjdk.java.net/)
* [Quarkus 1.11.0.Beta1](https://quarkus.io)

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
