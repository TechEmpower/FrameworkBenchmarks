# Blade Benchmarking Test

This is the Blade portion of a [benchmarking test suite](../) comparing a variety of web development platforms. The test utilizes Blade routes, JSON serialization, Blade-JDBC for ORM.

### Tests

* [Blade Application](src/main/java/hello/Application.java)
* [Database Model](src/main/java/hello/mode/World.java)

## Infrastructure Software Versions

* [Blade 2.0.3](https://github.com/biezhi/blade)
* [Java OpenJDK 1.8](http://openjdk.java.net/)
* [HikariCP 2.7.1](https://github.com/brettwooldridge/HikariCP)

## References

* https://github.com/netty/netty/tree/master/example/src/main/java/io/netty/example/http/snoop

## Test URLs

### JSON Encoding Test

http://localhost:9000/json 

### Data-Store/Database Mapping Test 

http://localhost:9000/db?queries=5

### Plain Text Test

http://localhost:9000/plaintext