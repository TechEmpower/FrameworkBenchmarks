#RestExpress Benchmarking Test

This is the Java RestExpress portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
For raw Servlets there is no broad consensus on JSON encoding so we have selected the fastest available JSON encoder for Java: [Jackson](http://wiki.fasterxml.com/JacksonHome).

* [JSON test source](src/main/java/hello/JsonController.java)

### MongoDB Data-Store/Database Mapping Test
* [DB test source](src/main/java/hello/MongodbController.java)

### MySQL Data-Store/Database Mapping Test
* [DB test source](src/main/java/hello/MysqlController.java)

## Infrastructure Software Versions
The tests were run with:

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Netty 4.0.29](http://netty.io)
* [Jackson 2.4.2](http://wiki.fasterxml.com/JacksonHome)
* [Morphia 1.0.1](https://github.com/mongodb/morphia)
* [RepoExpress 0.4.7](https://github.com/RestExpress/RepoExpress)
* [MySQL Connector 5.1.36](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/restexpress/json

### MySQL Database Mapping Test

http://localhost:8080/restexpress/mysql?queries=5

### MongoDB Database Mapping Test

http://localhost:8080/restexpress/mongodb?queries=5
