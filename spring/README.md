# Spring Benchmarking Test

This is the Spring portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
* [JSON test source](src/main/java/hello/web/HelloJsonController.java)

### Data-Store/Database Mapping Test
Use [Hibernate](http://www.hibernate.org/) for interaction with MySQL database.

* [Database test source](src/main/java/hello/web/HelloDbController.java)
* [Database Entity](src/main/java/hello/domain/World.java)
* [Hibernate Configuration](src/main/resources/hibernate.cfg.xml)

TODO add links to code

## Infrastructure Software Versions
The tests were run with:

* [Spring 3.2.1.RELEASE](http://www.springsource.org/)
* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Resin 4.0.34](http://www.caucho.com/)

## Resources
* http://blog.springsource.org/2011/01/04/green-beans-getting-started-with-spring-mvc/
* http://static.springsource.org/spring/docs/3.2.1.RELEASE/spring-framework-reference/html
* http://static.springsource.org/spring/docs/3.2.x/spring-framework-reference/html/orm.html#orm-hibernate

## Test URLs
### JSON Encoding Test

http://localhost:8080/spring/json

### Data-Store/Database Mapping Test

http://localhost:8080/spring/db?queries=5


