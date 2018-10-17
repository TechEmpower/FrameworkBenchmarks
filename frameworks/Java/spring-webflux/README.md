# Spring Webflux Benchmarking Test

This is the Spring Webflux portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

Netty is used for the async web server, with nearly everything configured with default settings. The only thing changed is Hikari can use up to (2 * cores count) connections (the default is 10). See [About-Pool-Sizing](https://github.com/brettwooldridge/HikariCP/wiki/About-Pool-Sizing)

A fixed thread pool of size equals to the number of database connections is used to run all the blocking code (jdbc database accesses) to not block netty's event loop.

For postgresql access, there are two implementations.
* [JdbcDbRepository](src/main/java/benchmark/JdbcDbRepository.java) is using JdbcTemplate.
* [PgClientDbRepository](src/main/java/benchmark/PgClientDbRepository.java) is using reactive-pg-client
For mongoDB access, spring-data-mongodb with reactive support is used. See [MongoDbRepository](src/main/java/benchmark/MongoDbRepository.java)

### Plaintext Test

* [Plaintext test source](src/main/java/benchmark/Controller/BenchmarkController.java)

### JSON Serialization Test

* [JSON test source](src/main/java/benchmark/Controller/BenchmarkController.java)

### Database Query Test

* [Query test source](src/main/java/benchmark/Controller/ReactiveController.java)

### Database Queries Test

* [Queries test source](src/main/java/benchmark/Controller/ReactiveController.java)

### Database Update Test

* [Update test source](src/main/java/benchmark/Controller/ReactiveController.java)

### Template rendering Test

* [Template rendering test source](src/main/java/benchmark/Controller/ReactiveController.java)

## Versions

* [Java OpenJDK 10](http://openjdk.java.net/)
* [Spring boot 2.0.5](https://spring.io/projects/spring-boot)
* [Spring data mongodb 2.0.9](https://projects.spring.io/spring-data-mongodb/)
* [reactive-pg-client 0.10.6](https://github.com/reactiverse/reactive-pg-client)

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
