# Quarkus Benchmarking Test

This is the Quarkus portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Implementations

There are currently 2 implementations:

- Quarkus using RESTEasy Reactive and Hibernate ORM (classic Hibernate for DB operations, while handling web via the reactive stack)
- Quarkus using RESTEasy Reactive and Hibernate Reactive (fully reactive stack)

## Testing

    ./tfb --mode verify --test quarkus quarkus-hibernate-reactive

After the database container image is created, it can be started independently.

    docker run -d --name tfb-database -e POSTGRES_DB=hello_world -e POSTGRES_PASSWORD=benchmarkdbpass -e POSTGRES_USER=benchmarkdbuser -p 5432:5432 techempower/postgres:latest

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
