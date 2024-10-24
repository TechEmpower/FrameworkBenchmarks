# Spring MVC Benchmarking Test

This is the Spring MVC portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

An embedded undertow is used for the web server.

There are two implementations :
* For postgresql access, JdbcTemplate is used. See [JdbcDbRepository](src/main/java/hello/JdbcDbRepository.java).
* For mongoDB access, MongoTemplate is used. See [MongoDbRepository](src/main/java/hello/MongoDbRepository.java).

### Plaintext Test

* [Plaintext test source](src/main/java/hello/HelloController.java)

### JSON Serialization Test

* [JSON test source](src/main/java/hello/HelloController.java)

### Database Query Test

* [Database Query test source](src/main/java/hello/HelloController.java)

### Database Queries Test

* [Database Queries test source](src/main/java/hello/HelloController.java)

### Database Update Test

* [Database Update test source](src/main/java/hello/HelloController.java)

### Template rendering Test

* [Template rendering test source](src/main/java/hello/HelloController.java)

## Versions

* [OpenJDK Runtime Environment Temurin-17](https://adoptium.net/es/temurin/releases/?version=17)
* [Spring-Boot 3.0.0](https://spring.io/projects/spring-boot)

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
