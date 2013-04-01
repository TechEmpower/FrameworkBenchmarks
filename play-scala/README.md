#Play Benchmarking Test

This is the Play portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](app/controllers/Application.java)

### Data-Store/Database Mapping Test

* [Database test controller](app/controllers/Application.scala)
* [Database test model](app/models/World.java)

## Infrastructure Software Versions
The tests were run with:

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Play 2.1.0](http://http://www.playframework.com/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db?queries=5
