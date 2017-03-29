# RingoJs Benchmarking Test

This is the Ringojs portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test controller/view](ringo-main.js)

### Data-Store/Database Mapping Test

* [DB test controller/model](ringo-main.js)

## Infrastructure Software Versions

The tests were run with:

* [RingoJs v1.1](http://ringojs.org/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/json

### Data-Store/Database Mapping Test

MySQL:

http://localhost:8080/db

### Variable Query Test

MySQL:
http://localhost:8080/db?queries=2
