# Grails Benchmarking Test

This is the Grails portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Infrastructure Software Versions
The tests were run with:
* [Grails 2.3.6](http://grails.org/)
* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Resin 4.0.34](http://www.caucho.com/)
* [MySQL 5.5.29](https://dev.mysql.com/)


## Test URLs

### Test type 1: JSON serialization

http://localhost:8080/grails/hello/json

### Test type 2: Single database query

http://localhost:8080/grails/hello/db

### Test type 3: Multiple database queries

http://localhost:8080/grails/hello/queries?queries=10

### Test type 4: Fortunes

http://localhost:8080/grails/hello/fortunes

### Test type 5: Database updates

http://localhost:8080/grails/hello/updates?queries=10

### Test type 6: Plaintext

http://localhost:8080/grails/hello/plaintext
