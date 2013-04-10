#Lift Benchmarking Test (stateless)

This is the stateless Lift portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](src/main/scala/code/lib/StatelessJson.scala)

### Data-Store/Database Mapping Test

* [Database test snippet](src/main/scala/code/lib/StatelessDb.scala)
* [Database test model](src/main/scala/code/model/World.scala)

## Infrastructure Software Versions
The tests were run with:

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Lift 2.5.0-RC2](http://http://www.liftweb.net/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db/5
