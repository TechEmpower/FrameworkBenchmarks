# Play Benchmarking Test

This is the Play portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### Data-Store/Database Mapping Test

* [Database test controller](app/controllers/Application.java)
* [Database World test model](app/models/tables/World.java)
* [Database Fortune test model](app/models/tables/Fortune.java)

## Infrastructure Software Versions
The tests were run with:

* Java 8
* [Play 2.7.0](https://www.playframework.com/)

## Test URLs
### Data-Store/Database Mapping Test

* http://localhost/db
* http://localhost/queries?queries=10
* http://localhost/fortunes
* http://localhost/update?queries=10
