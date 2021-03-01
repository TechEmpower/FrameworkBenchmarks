# Helidon Benchmarking Test

This is the Helidon portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

There is currently one repository implementation.
* [JdbcRepository](src/main/java/io/helidon/benchmark/models/JdbcRepository.java) is using JDBC and an io thread pool of size (2 * cores count) to prevent blocking netty's main event loop. It uses hikaricp to manage the connection pool. It is configured for a maximum of (2 * cores count) concurrent connections. See [About-Pool-Sizing](https://github.com/brettwooldridge/HikariCP/wiki/About-Pool-Sizing) for more information. I am assuming that the DB is running on a computer with the same spec as the one running the app, which seems to be the case based on what is written [here](https://www.techempower.com/benchmarks/#section=environment&hw=ph&test=db&l=fjd9b3)

### Plaintext Test

* [Plaintext test source](src/main/java/io/helidon/benchmark/services/PlainTextService.java)

### JSON Serialization Test

* [JSON test source](src/main/java/io/helidon/benchmark/services/JsonService.java)

### Database Query Test

* [Database Query test source](src/main/java/io/helidon/benchmark/services/DbService.java)

### Database Queries Test

* [Database Queries test source](src/main/java/io/helidon/benchmark/services/DbService.java)

### Database Update Test

* [Database Update test source](src/main/java/io/helidon/benchmark/services/DbService.java)

### Template rendering Test

* [Template rendering test source](src/main/java/io/helidon/benchmark/services/FortuneService.java)

## Versions

* [Java OpenJDK 11](http://openjdk.java.net/)
* [Helidon 2.2.1](http://helidon.io/)

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
