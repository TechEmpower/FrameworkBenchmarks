#Cognition Benchmarking Test

This is the Cognition portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](src/main/scala/app/JsonController.scala)

### Data-Store/Database Mapping Test

* [Database test controller and model](src/main/scala/app/DbController.scala)

## Infrastructure Software Versions
The tests were run with:

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost:8000/json

### Data-Store/Database Mapping Test

http://localhost:8000/db?count=

### Variable Query Test

http://localhost:8000/queries?count=

