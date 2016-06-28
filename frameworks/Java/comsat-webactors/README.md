# Parallel Universe Comsat Web Actors Netty Benchmarking Test

This is the [Parallel Universe Comsat Web Actors](http://docs.paralleluniverse.co/comsat/#servlets) version of a [benchmarking test suite](../) comparing a variety of web development platforms. It serves requests in Erlang-like actors running on lightweight [Quasar Fibers](http://docs.paralleluniverse.co/quasar/#fibers) rather than heavyweight Java threads and it can run on any servlet container (tested regularly on Undertow, Jetty and Tomcat) as well as an Undertow or Netty handler (present configuration).

### JSON and Plaintest Encoding Tests

* [JSON test source](src/main/java/hello/HelloWebActor.java)

## Versions

* Jackson JSON 2.7.0 (https://github.com/FasterXML/jackson)
* Quasar 0.7.4 (http://docs.paralleluniverse.co/quasar)
* Comsat 0.6.0-SNAPSHOT (http://docs.paralleluniverse.co/comsat)
* Netty 4.0.33.Final (http://undertow.io)

## Test URLs

### JSON Encoding Test

    http://localhost:8080/json

### Plaintest

    http://localhost:8080/plaintest
