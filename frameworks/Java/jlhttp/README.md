# JLHTTP Benchmarking Test

This is the JLHTTP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

The main goal of [JLHTTP - the Java Lightweight HTTP Server](http://www.freeutils.net/source/jlhttp/)
is to be truly lightweight, i.e. as small as a fully functional and RFC-compliant Java HTTP server can be.
With all else being equal, it does strive for high performance - but usually all else is not equal,
and a trade-off must be made between size (complexity) and performance.
JLHTTP usually sides with the smaller size.


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
