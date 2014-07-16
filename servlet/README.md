#Servlet Benchmarking Test

This is the Java Servlet portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
For raw Servlets there is no broad consensus on JSON encoding so we have selected the fastest available JSON encoder for Java: [Jackson](http://wiki.fasterxml.com/JacksonHome).

* [JSON test source](src/main/java/hello/JsonServlet.java)

### Data-Store/Database Mapping Test
* [DB test source](src/main/java/hello/DbPoolServlet.java)

## Infrastructure Software Versions
The tests were run with:

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Resin 4.0.34](http://www.caucho.com/)
* [Jackson 2.3.0](http://wiki.fasterxml.com/JacksonHome)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/servlet/json

### Data-Store/Database Mapping Test

http://localhost:8080/servlet/db?queries=5

