# Blade Benchmarking Test

This is the Blade portion of a [benchmarking test suite](../) comparing a variety of web development platforms. The test utilizes Blade routes, JSON serialization, [Anima](https://github.com/biezhi/anima) for ORM.

### Tests

* [Blade Application](src/main/java/hello/Application.java)
* [Database Model](src/main/java/hello/mode/World.java)

## Infrastructure Software Versions

* [Blade 2.1.2.RELEASE](https://github.com/lets-blade/blade)
* [Java OpenJDK 1.8](http://openjdk.java.net/)
* [HikariCP 4.0.3](https://github.com/brettwooldridge/HikariCP)

## Test URLs

### JSON Encoding Test

http://localhost:9000/json 

### Data-Store/Database Mapping Test 

http://localhost:9000/db?queries=5

### Plain Text Test

http://localhost:9000/plaintext

### Update Test

http://localhost:9000/updates?queries=2

### Fortunes Test

http://localhost:9000/fortunes