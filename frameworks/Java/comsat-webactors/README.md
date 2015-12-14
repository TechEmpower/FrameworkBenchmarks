# Parallel Universe Comsat Web Actors Benchmarking Test

This is the [Parallel Universe Comsat Web Actors](http://docs.paralleluniverse.co/comsat/#servlets) version of a [benchmarking test suite](../) comparing a variety of web development platforms. It serves requests in Erlang-like actors running on lightweight [Quasar fibers](http://docs.paralleluniverse.co/quasar/#fibers) rather than heavyweight Java threads and it can run on any servlet container (configurations provided for embedded Undertow, Jetty and Tomcat) as well as a Netty and Undertow handler (default configuration).

### JSON and Plaintest Encoding Tests

* [JSON test source](src/main/java/hello/HelloWebActor.java)

## Versions

* Jackson JSON 2.5.4 (https://github.com/FasterXML/jackson)
* Quasar 0.7.3 (http://docs.paralleluniverse.co/quasar)
* Comsat 0.5.0 (http://docs.paralleluniverse.co/comsat)
* Undertow 1.2.10-final (http://undertow.io)
* Netty 4.0.30.Final (http://undertow.io)
* Tomcat 8.0.26 (http://tomcat.apache.org)
* Jetty 9.2.13.v20150730 (http://www.eclipse.org/jetty/)

## Test URLs

### JSON Encoding Test

    http://localhost:8080/json

### Plaintest

    http://localhost:8080/plaintest
