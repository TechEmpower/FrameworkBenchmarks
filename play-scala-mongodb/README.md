#Play Benchmarking Test

This is the Play portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](app/controllers/Application.scala)

### Data-Store/Database Mapping Test

* [Database test controller](app/controllers/Application.scala)

## Infrastructure Software Versions
The tests were run with:

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Play 2.1.0](http://http://www.playframework.com/)
* [Reactivemongo 0.9-SNAPSHOT](https://github.com/zenexity/Play-ReactiveMongo)

## Test URLs
### Data-Store/Database Mapping Test

http://localhost/db?queries=5
