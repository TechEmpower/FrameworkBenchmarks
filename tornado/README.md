# Tornado Benchmarking Test

This is the Tornado portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](server.py)


### Data-Store/Database Mapping Test

* [Database teste source Raw](server.py)

## Infrastructure Software Versions
The tests were run with:
* [Python 2.7.3](http://www.python.org/)
* [Tornado 3](https://www.tornadoweb.com/)
* [nginx 1.2.7](http://nginx.org/)
* [Mongodb 2.0.4](https://www.mongodb.org/)


## Resources
* http://www.tornadoweb.org/en/stable/documentation.html

## Test URLs
### JSON Encoding Test

http://localhost:8080/json

### Data-Store/Database Mapping Test

http://localhost:8080/db

### Variable Query Test

http://localhost:8080/db?queries=2
