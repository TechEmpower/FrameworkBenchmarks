#Scalatra Benchmarking Test

This is the Scalatra portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
This example uses the built-in [JacksonJsonSupport](http://scalatra.org/2.2/guides/formats/json.html)

* [JSON test source](src/main/scala/hello/JsonController.scala)

### Data-Store/Database Mapping Test
* [DB test source](src/main/scala/hello/DbController.scala)

## Infrastructure Software Versions
The tests were run with:

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Scalatra 2.2.0](http://www.scalatra.org/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/json

### Data-Store/Database Mapping Test

http://localhost:8080/db?queries=5

