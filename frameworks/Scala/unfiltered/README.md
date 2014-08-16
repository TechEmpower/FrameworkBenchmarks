#Unfiltered Benchmarking Test

This is the Unfiltered portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](src/main/scala/Plans.scala)

### Data-Store/Database Mapping Test

* [Database test controller and model](src/main/scala/Plans.scala)

## Infrastructure Software Versions
The tests were run with:

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Unfiltered 0.6.8](http://unfiltered.databinder.net/Unfiltered.html)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db?queries=5
