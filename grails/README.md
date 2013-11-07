# Grails Benchmarking Test

This is the Grails portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Infrastructure Software Versions
The tests were run with:
* [Grails 2.3.2](http://grails.org/)
* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Resin 4.0.34](http://www.caucho.com/)
* [MySQL 5.5.29](https://dev.mysql.com/)


## Test URLs
### JSON Encoding Test

http://localhost:8080/grails/hello/json

### Data-Store/Database Mapping Test

http://localhost:8080/grails/hello/queries?queries=5
