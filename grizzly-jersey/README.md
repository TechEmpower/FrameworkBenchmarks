# Grizzly+Jersey Benchmarking Test

This is the Grizzly+Jersey portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON serialization test
* [JSON test resource](src/main/java/hello/JsonResource.java)

### Database tests
* [Database tests resource](src/main/java/hello/DbResource.java)

### Fortunes test
* [Fortunes test resource](src/main/java/hello/FortunesResource.java)

## Versions

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Jersey 1.17.1](http://jersey.java.net/)
* [Grizzly 2.3.2](http://grizzly.java.net/)
* [Jackson 2.2.1](http://wiki.fasterxml.com/JacksonHome)
* [Mustache.java 0.8.12](https://github.com/spullara/mustache.java)
* [Jersey Mustache 1.0.0](https://github.com/trautonen/jersey-mustache)
* [Hibernate 4.2.1](http://www.hibernate.org/)

## Test URLs

### JSON serialization test

    http://localhost:8080/json

### Database tests

    http://localhost:8080/db

    http://localhost:8080/db?queries=10

### Fortunes test

    http://localhost:8080/fortunes
