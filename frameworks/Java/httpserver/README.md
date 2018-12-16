# httpserver Benchmarking Test

This is the com.sun.net.httpserver portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

Package [com.sun.net.httpserver](https://docs.oracle.com/javase/8/docs/jre/api/net/httpserver/spec/com/sun/net/httpserver/HttpServer.html)
provides a simple high-level Http server API, which can be used to build embedded HTTP servers.
It is built-in to the Oracle JDK and OpenJDK (but is not part of the Java standard
and is not available in other JDKs).




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
