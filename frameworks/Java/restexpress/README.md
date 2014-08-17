#RestExpress Benchmarking Test

This is the Java RestExpress portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
For raw Servlets there is no broad consensus on JSON encoding so we have selected the fastest available JSON encoder for Java: [Jackson](http://wiki.fasterxml.com/JacksonHome).

* [JSON test source](src/main/java/hello/JsonServlet.java)

### Data-Store/Database Mapping Test
* [DB test source](src/main/java/hello/DBServlet.java)

## Infrastructure Software Versions
The tests were run with:

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Netty 3.6.2](http://netty.io)
* [Jackson 2.1.4](http://wiki.fasterxml.com/JacksonHome)
* [Morphia 1.2.2](https://github.com/jmkgreen/morphia)
* [RepoExpress 0.3.2](https://github.com/RestExpress/RepoExpress)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/restexpress/json

### MySQL Database Mapping Test

http://localhost:8080/restexpress/mysql?queries=5

### MongoDB Database Mapping Test

http://localhost:8080/restexpress/mongodb?queries=5

