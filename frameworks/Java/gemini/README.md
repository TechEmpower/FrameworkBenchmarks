# Gemini Benchmarking Test

This is the Gemini portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](Source/hello/home/handler/HelloHandler.java)

### Data-Store/Database Mapping Test

* [DB test controller](Source/hello/home/handler/HelloHandler.java)
* [DB test model](Source/hello/home/entity/World.java)


## Infrastructure Software Versions
The tests were run with:
* [Oracle Java 1.8](https://www.oracle.com/java/)
* [Resin 4.0.55](http://www.caucho.com/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/

### Data-Store/Database Mapping Test

http://localhost:8080/db

### Variable Query Test

http://localhost:8080/db?queries=2
