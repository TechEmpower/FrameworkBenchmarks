# Spring Benchmarking Test

This is the Spring portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

This is using several [Spring projects](https://spring.io/projects) to build an application.

## How to run this app locally

You can easily run this sample application with an embedded H2 database.
For that, simply run:

    SPRING_PROFILES_ACTIVE=local mvn spring-boot:run

## Guides
* [Building a REST service](https://spring.io/guides/gs/rest-service/)
* [Building an application with Spring Boot](https://spring.io/guides/gs/spring-boot/)
* [Accessing data with JPA](https://spring.io/guides/gs/accessing-data-jpa/)

## Documentation
See [Spring projects documentation](https://spring.io/docs).

## Application Endpoints

When deployed locally, the application uses the 'ROOT' context; for the benchmark, all
URLs are prefixed with "/spring".
Check out [SampleApplication, the main Application file](src/main/java/com/techempower/spring/SampleApplication.java)

### JSON serialization

* http://localhost:8080/json
* [JSON Controller](src/main/java/com/techempower/spring/web/HelloController.java)

### Single database query

* http://localhost:8080/db
* [Database Controller](src/main/java/com/techempower/spring/web/WorldDatabaseController.java)
* [Database Entity](src/main/java/com/techempower/spring/domain/World.java)
* [Database Repository](src/main/java/com/techempower/spring/service/WorldRepository.java)

### Multiple database queries

* http://localhost:8080/queries?queries=5
* [Database Controller](src/main/java/com/techempower/spring/web/WorldDatabaseController.java)

### Fortunes

* http://localhost:8080/fortunes
* [Fortune Controller](src/main/java/com/techempower/spring/web/FortuneController.java)

### Database updates

* http://localhost:8080/updates?queries=5
* [Database Controller](src/main/java/com/techempower/spring/web/WorldDatabaseController.java)

### Plaintext

* http://localhost:8080/plaintext
* [Controller](src/main/java/com/techempower/spring/web/HelloController.java)


## Infrastructure Software Versions
The tests were run with:

* [Spring 4.0.2.RELEASE](http://projects.spring.io/spring-framework/)
* [Spring Boot 1.0.0.RC4](http://projects.spring.io/spring-boot/)
* [Spring Data JPA 1.5.0.RELEASE](http://projects.spring.io/spring-data-jpa/)
* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Tomcat 8.0.3](https://tomcat.apache.org/)