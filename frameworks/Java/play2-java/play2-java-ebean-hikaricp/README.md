#Play Benchmarking Test

This is the Play portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test controller](app/controllers/Application.java)

### Data-Store/Database Mapping Test

* [Database test controller](app/controllers/Application.java)
* [Database World test model](app/models/World.java)
* [Database Fortune test model](app/models/Fortune.java)

### Plain Text Test

* [Plain text test controller](app/controllers/Application.java)

## Infrastructure Software Versions
The tests were run with:

* Java 8
* [Play 2.4.2](http://http://www.playframework.com/)

## Test URLs
### JSON Encoding Test

* http://localhost/json

### Data-Store/Database Mapping Test

* http://localhost/db
* http://localhost/queries?queries=10
* http://localhost/fortunes
* http://localhost/update?queries=10
