# Parallel Universe Comsat Benchmarking Test

This is the [Parallel Universe Comsat](http://docs.paralleluniverse.co/comsat/#overview) version of a [benchmarking test suite](../) comparing a variety of web development platforms. It serves requests in lightweight [Quasar fibers](http://docs.paralleluniverse.co/quasar/#fibers) rather than heavyweight Java threads. There are two main flavors: Comsat Servlet and Comsat Web Actors.

## Comsat Servlet

[Parallel Universe Comsat Servlet](http://docs.paralleluniverse.co/comsat/#servlets) works with any servlet containers (tests are run against embedded Undertow and Jetty).

### JSON Encoding and Plaintext Tests

* [JSON test source](src/main/java/hello/servlet/JsonServlet.java)
* [Plaintext test source](src/main/java/hello/servlet/PlaintextServlet.java)

## Comsat Web Actors

[Parallel Universe Comsat Web Actors](http://docs.paralleluniverse.co/comsat/#web-actors) runs as an Undertow or Netty handler.

### JSON and Plaintest Encoding Tests

* [JSON test source](src/main/java/hello/webactors/HelloWebActor.java)

## Versions

* Jackson JSON 2.7.4 (https://github.com/FasterXML/jackson)
* Quasar 0.7.5 (http://docs.paralleluniverse.co/quasar)
* Comsat 0.7.0 (http://docs.paralleluniverse.co/comsat)
* Jetty 9.3.9.v20160517 (https://www.eclipse.org/jetty/)
* Netty 4.0.36.Final (http://netty.io/)
* Undertow 1.3.22-final (http://undertow.io)

## Test URLs

### JSON Encoding Test

    http://localhost:8080/json

### Plaintest

    http://localhost:8080/plaintest
