# Redkale Benchmarking Test

This is the Redkale portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### Plaintext Test

* [Plaintext test source](src/main/java/org/redkalex/benchmark/Servlet.java)

### JSON Serialization Test

* [JSON test source](src/main/java/org/redkalex/benchmark/Servlet.java)

### Database Query Test

* [Database Query test source](src/main/java/org/redkalex/benchmark/Servlet.java)

### Database Queries Test

* [Database Queries test source](src/main/java/org/redkalex/benchmark/Servlet.java)

### Database Update Test

* [Database Update test source](src/main/java/org/redkalex/benchmark/Servlet.java)

### Template rendering Test

* [Template rendering test source](src/main/java/org/redkalex/benchmark/Servlet.java)

## Versions

* [Java OpenJDK 1.8](http://openjdk.java.net/)
* [Redkale 1.9.3](http://redkale.org/)

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
