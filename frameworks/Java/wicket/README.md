# Wicket Benchmarking Test

This is the wicket portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
* [JSON test source](src/main/java/hellowicket/HelloJsonResponse.java) - uses Jackson + Afterburner module

### Data-Store/Database Mapping Test
Use JDBC for interaction with MySQL database.

* [Database test source](src/main/java/hellowicket/HelloDbResponse.java)
* [Database Entity](src/main/java/hellowicket/World.java)

## Versions

* [Wicket 9.6.0](http://wicket.apache.org/)
* [MySQL 5.5.+](http://dev.mysql.com/)
* [Jackson 2.13.x](http://wiki.fasterxml.com/JacksonHome)

## Test URLs

### JSON Encoding Test

    http://localhost:8080/wicket/json

### Database Mapping Test

    http://localhost:8080/wicket/db?queries=5
