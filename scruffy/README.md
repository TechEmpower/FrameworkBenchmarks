#Scruffy Benchmarking Test

This is the Scruffy portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
This example uses the built-in Jackson for json support.

* [JSON test source](src/main/scala/hello/JsonController.scala)

### Data-Store/Database Mapping Test
* [DB test source](src/main/scala/hello/DbController.scala)

## Infrastructure Software Versions
The tests were run with:

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Scruffy 1.3.11](http://scruffy-project.github.io/)

## Test URLs

### JSON Encoding Test

http://localhost:8080/json

### Data-Store/Database Mapping Test

http://localhost:8080/db?queries=5

