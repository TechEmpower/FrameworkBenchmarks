#Play Benchmarking Test

This is the Play portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](app/controllers/Application.java)

### Data-Store/Database Mapping Test

* [Database test controller](app/controllers/Application.java)
* [Database test model](app/models/World.java)

## Infrastructure Software Versions
The tests were run with:

* [Java OpenJDK 1.7](http://openjdk.java.net/)
* [Play 2.3.3](http://http://www.playframework.com/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db
http://localhost/queries?queries=10