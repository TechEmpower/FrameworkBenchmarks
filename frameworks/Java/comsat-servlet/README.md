# Parallel Universe Comsat Servlet Undertow Benchmarking Test

This is the [Parallel Universe Comsat Servlet](http://docs.paralleluniverse.co/comsat/#servlets) version of a [benchmarking test suite](../) comparing a variety of web development platforms. It serves requests in lightweight [Quasar fibers](http://docs.paralleluniverse.co/quasar/#fibers) rather than heavyweight Java threads and it works with any servlet containers (tests are run against embedded Undertow, Jetty and Tomcat). The servlet container used in this benchmark is Undertow.

### JSON Encoding and Plaintext Tests

* [JSON test source](src/main/java/hello/JsonServlet.java)
* [Plaintext test source](src/main/java/hello/PlaintextServlet.java)

## Versions

* Jackson JSON 2.7.0 (https://github.com/FasterXML/jackson)
* Quasar 0.7.4 (http://docs.paralleluniverse.co/quasar)
* Comsat 0.6.0-SNAPSHOT (http://docs.paralleluniverse.co/comsat)
* Undertow 1.3.15-final (http://undertow.io)

## Test URLs

### JSON Encoding Test

    http://localhost:8080/json

### Plaintest

    http://localhost:8080/plaintest
