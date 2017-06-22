# Jetty-9 Benchmarking Test

This is the Jetty portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Handler

### JSON Encoding Test
* [Plaintext test source](src/main/java/hello/handler/PlainTextHandler.java)
* [JSON test source](src/main/java/hello/handler/JsonHandler.java)

## Servlet

### JSON Encoding Test
* [Plaintext test source](src/main/java/hello/handler/PlaintextServlet.java)
* [JSON test source](src/main/java/hello/servlet/JsonServlet.java)

## Versions
9.4.6.v20170531 (http://eclipse.org/jetty)

## Test URLs

All implementations use the same URLs.

 * Plaintext - `http://localhost:8080/plaintext`
 * JSON - `http://localhost:8080/json`
