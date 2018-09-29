# Spring Webflux Benchmarking Test

This is the Spring Webflux portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

Netty is used for the async web server, with nearly everything configured with default settings. The only thing changed is Hikari can use up to (2 * cores count) connections (the default is 10). See [About-Pool-Sizing](https://github.com/brettwooldridge/HikariCP/wiki/About-Pool-Sizing)

A fixed thread pool of size equals to the number of database connections is used to run all the blocking code (postgresql database accesses) to not block netty's event loop.

For postgresql access, JdbcTemplate is used.
For mongoDB access, spring-data-mongodb with reactive support is used.

### Plaintext Test

* [Plaintext test source](src/main/java/benchmark/BenchmarkController.java)

### JSON Serialization Test

* [JSON test source](src/main/java/benchmark/BenchmarkController.java)

### Database Query Test

* [Postgresql Query test source](src/main/java/benchmark/SQLController.java)
* [MongoDB Query test source](src/main/java/benchmark/NoSQLController.java)

### Database Queries Test

* [Postgresql Queries test source](src/main/java/benchmark/SQLController.java)
* [MongoDB Queries test source](src/main/java/benchmark/NoSQLController.java)

### Database Update Test

* [Postgresql Update test source](src/main/java/benchmark/SQLController.java)
* [MongoDB Update test source](src/main/java/benchmark/NoSQLController.java)

### Template rendering Test

* [Postgresql Template rendering test source](src/main/java/benchmark/SQLController.java)
* [MongoDB Template rendering test source](src/main/java/benchmark/NoSQLController.java)

## Versions

* [Java OpenJDK 10](http://openjdk.java.net/)
* [Spring boot 2.0.4](https://spring.io/projects/spring-boot)
* [Spring data mongodb 2.0.9](https://projects.spring.io/spring-data-mongodb/)

## Test URLs

### Plaintext Test

    http://localhost:8080/plaintext

### JSON Encoding Test

    http://localhost:8080/json

### Database Query Test

    http://localhost:8080/db
    http://localhost:8080/mongo/db

### Database Queries Test

    http://localhost:8080/queries?queries=5
    http://localhost:8080/mongo/queries?queries=5

### Database Update Test

    http://localhost:8080/updates?queries=5
    http://localhost:8080/mongo/updates?queries=5

### Template rendering Test

    http://localhost:8080/fortunes
    http://localhost:8080/mongo/fortunes
