#Play Benchmarking Test

This is the Play portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### Data-Store/Database Mapping Test

* [Database test controller](app/controllers/Application.scala)

## Infrastructure Software Versions
The tests were run with:

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Play 2.2.0](http://http://www.playframework.com/)
* [Reactivemongo 0.10.5.0.akka22](https://github.com/zenexity/Play-ReactiveMongo)

## Test URLs
### Single Database Query test
http://localhost:9000/db
### Single Database Query test
http://localhost:9000/queries?queries=5
