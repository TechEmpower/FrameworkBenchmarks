# Grails Benchmarking Test

This is the Grails portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test controller/view](hello/grails-app/HelloController.groovy) TODO: this gives 404

### Data-Store/Database Mapping Test

* [DB test controller](hello/grails-app/HelloController.groovy) TODO: this gives 404
* [DB test model](hello/grails-app/domain/hello/World.groovy) TODO: this gives 404

## Infrastructure Software Versions
The tests were run with:
* [Grails 2.1.1](http://grails.org/)
* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Resin 4.0.34](http://www.caucho.com/)
* [MySQL 5.5.29](https://dev.mysql.com/)


## Test URLs
### JSON Encoding Test

http://localhost:8080/grails/hello/json

### Data-Store/Database Mapping Test

http://localhost:8080/grails/hello/db?queries=5