# httpserver Benchmarking Test

This is an alternative version of the [httpserver benchmarking test suite](../httpserver)

Package [robaho.net.httpserver](https://github.com/robaho/httpserver) provides an implementation of `com.sun.net.httpserver` designed for virtual threads, thus requiring JDK21+.

It can be used with platform threads using a `cached thread pool` which configures a thread per task, which is more efficient for a small number of clients (embedded systems).

### Test Type Implementation Source Code

* [JSON](src/main/java/benchmarks/Server.java)
* [Plaintext](src/main/java/benchmarks/Server.java)
* [Fortunes](src/main/java/benchmarks/Server.java)

## Important Libraries
The tests were run with:
* [Jackson](https://github.com/FasterXML/jackson)
* [HikariCP](https://github.com/brettwooldridge/HikariCP)
* [HTTL](https://httl.github.io/en/)

## Test URLs
### JSON

http://localhost:8080/json

### Plaintext

http://localhost:8080/plaintext

### Fortunes

http://localhost:8080/fortunes
