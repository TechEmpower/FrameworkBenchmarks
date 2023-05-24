# Quarkus Benchmarking Test

This is the Quarkus portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Implementations

There are currently 3 implementations:

- RESTEasy and Hibernate ORM
- RESTEasy Reactive and Hibernate ORM
- RESTEasy Reactive and Hibernate Reactive

## Versions

* [Java OpenJDK 17](http://openjdk.java.net/)
* [Quarkus 3.1.0.CR1](https://quarkus.io)

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
