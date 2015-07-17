# Parallel Universe Comsat Servlet on Undertow Benchmarking Test

This is the [Parallel Universe Comsat Servlet](http://docs.paralleluniverse.co/comsat/#servlets) version of a [benchmarking test suite](../) comparing a variety of web development platforms. It serves requests in lightweight [Quasar fibers](http://docs.paralleluniverse.co/quasar/#fibers) rather than heavyweight Java threads and it currently runs on an embedded Undertow server (but it can run on any servlet container).

### JSON Encoding Test

* [JSON test source](src/main/java/hello/HelloWebServer.java)

## Versions

* Jackson JSON 2.5.4 (https://github.com/FasterXML/jackson)
* Quasar 0.7.2 (http://docs.paralleluniverse.co/quasar)
* Comsat 0.4.0 (http://docs.paralleluniverse.co/comsat)
* Undertow 1.2.8-final (http://undertow.io)

## Test URLs

### JSON Encoding Test

    http://localhost:8080/json

### Plaintest

    http://localhost:8080/plaintest
