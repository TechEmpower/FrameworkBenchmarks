#Play Benchmarking Test

This is the Play portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### Data-Store/Database Mapping Test

* [Database test controller](app/controllers/Application.scala)
* [Database test model](app/models/World.scala)

## Infrastructure Software Versions
The tests were run with:

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Play 2](http://http://www.playframework.com/)

## Test URLs

### Data-Store/Database Mapping Test

http://localhost/db?queries=5
