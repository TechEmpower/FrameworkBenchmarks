#Scruffy Benchmarking Test

This is the Scruffy portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Infrastructure Software Versions

The tests were run with:

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Scruffy 1.13.0](http://scruffy-project.github.io/)

## Test URLs

### Test type 1: JSON serialization

http://localhost:8080/json

This example uses the built-in Jackson for json support.

* [Test 1 source](src/main/scala/scruffy/examples/Test1Endpoint.scala)

### Test type 2: Single database query

This example uses casbah for Mongo queries. Future improvement would be to switch to a nonblocking library.

* [Test 2 source](src/main/scala/scruffy/examples/Test2Endpoint.scala)

http://localhost:8080/db

### Test type 6: Plaintext

http://localhost:8080/plaintext

* [Test 6 source](src/main/scala/scruffy/examples/Test6Endpoint.scala)