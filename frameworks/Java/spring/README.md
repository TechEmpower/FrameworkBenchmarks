# Spring MVC Benchmarking Test

This is the Spring MVC portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

An embedded tomcat is used for the web server, with nearly everything configured with default settings.
The only thing changed is Hikari can use up to (2 * cores count) connections (the default is 10).
See [About-Pool-Sizing](https://github.com/brettwooldridge/HikariCP/wiki/About-Pool-Sizing)

Tomcat use a fixed thread pool that can grow up to 200 threads.

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

* [Java OpenJDK 11](http://openjdk.java.net/)
* [Spring boot 2.1.2](https://spring.io/projects/spring-boot)

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
