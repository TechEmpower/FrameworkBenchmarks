# JLHTTP Benchmarking Test

This is the JLHTTP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

The main goal of [JLHTTP - the Java Lightweight HTTP Server](http://www.freeutils.net/source/jlhttp/)
is to be truly lightweight, i.e. as small as a fully functional and RFC-compliant Java HTTP server can be.
With all else being equal, it does strive for high performance - but usually all else is not equal,
and a trade-off must be made between size (complexity) and performance.
JLHTTP usually sides with the smaller size.


### Plaintext Test

* [Plaintext test source](src/main/java/hello/HelloWebServer.java)

### JSON Encoding Test

The JSON encoding is performed using Jackson.

* [JSON test source](src/main/java/hello/HelloWebServer.java)


## Test URLs

### Plaintext Test

http://localhost:8080/plaintext

### JSON Encoding Test

http://localhost:8080/json
