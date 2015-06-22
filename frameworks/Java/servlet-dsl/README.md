#DSL Servlet Benchmarking Test

This servlet uses precompiled DSL model for endpoint communication.

### DSL model
Data structure is defined in an DSL schema

* [DSL source](src/main/java/dsl/model.dsl)

### JSON Encoding Test
DSL client Java is used for JSON encoding.

* [JSON test source](src/main/java/dsl/JsonServlet.java)

## Infrastructure Software Versions
The tests were run with:

* [Oracle Java 1.7](https://www.oracle.com/java/)
* [Resin 4.0](http://www.caucho.com/)
* [DSL client Java 1.3](http://github.com/ngs-doo/dsl-client-java)

## Test URLs
### JSON Encoding Test

http://localhost:8080/servlet-dsl/json

### Plaintext Test

http://localhost:8080/servlet-dsl/plaintext
