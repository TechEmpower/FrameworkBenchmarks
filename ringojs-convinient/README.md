# RingoJs Benchmarking Test

This is the Ringojs with the Stick framework portion of the [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test controller/view](ringo-main.js)

### Data-Store/Database Mapping Test

* [DB test controller/model](ringo-main.js)

## Infrastructure Software Versions

The tests were run with:

* [RingoJs v0.9](http://ringojs.org/)
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
