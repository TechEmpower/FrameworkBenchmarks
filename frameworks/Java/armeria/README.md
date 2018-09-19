# Armeria Benchmarking Test

This is the armeria portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Infrastructure Software Versions

* [Armeria 0.71.1](https://line.github.io/armeria/)
* [HikariCP 2.7.8](https://github.com/brettwooldridge/HikariCP)
* [Postgresql 42.1.4](https://jdbc.postgresql.org/)
* [Mustache 0.9.5](https://mustache.github.io/)

## Source for Tests

* [JSON serialization test source](src/main/java/hello/services/HelloService.java)
* [Single database query test source](src/main/java/hello/services/DbService.java)
* [Multiple database query test source](src/main/java/hello/services/DbService.java)
* [Fortunes test source](src/main/java/hello/services/FortunesService.java)
* [Database test source](src/main/java/hello/services/DbService.java)
* [Plaintext test source](src/main/java/hello/HelloService.java)

## Test URLs

### JSON Serialization Test

    http://localhost:8080/json

### Single Database Query

    http://localhost:8080/db

### Multiple Database Query

    http://localhost:8080/queries/

### Fortunes

    http://localhost:8080/fortunes

### Database Updates

    http://localhost:8080/updates/

### Plaintext Test

    http://localhost:8080/plaintext
