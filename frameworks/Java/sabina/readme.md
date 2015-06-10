
# Sabina Benchmarking Test

This is the Sabina portion of a [benchmarking test suite](../) comparing a variety of web
development platforms. The test utilizes Sabina routes, Gson for JSON serialization and a custom
OSIV pattern created with Sabina filters.


## Tests

* [Sabina application](/src/main/java/sabina/benchmark/Application.java)


## Infrastructure Software Versions

* [Sabina 1.0](http://there4.co/)


## Different test setups

* Local environment with Sabina's built in embedded Jetty (port=8080, context=/)
 * Start application from [Application](/src/main/java/sabina/benchmark/Application.java)'s main method
* Local environment with Sabina's built in embedded Undertow (port=8080, context=/)
 * Start application from [Application](/src/main/java/sabina/benchmark/Application.java)'s main method


## Test URLs

### JSON Encoding Test

http://localhost:8080/json

### Data-Store/Database Mapping Test

http://localhost:8080/db?queries=5

### Plain Text Test

http://localhost:8080/plaintext

