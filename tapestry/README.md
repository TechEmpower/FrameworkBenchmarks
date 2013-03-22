# Tapestry Benchmarking Test

This is the Tapestry portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Use the Tapestry standard [JSON encoder](https://tapestry.apache.org/tapestry5/apidocs/org/apache/tapestry5/json/JSONObject.html)

* [JSON test source](hello/src/main/java/hello/pages/HelloJSON.java)

### Data-Store/Database Mapping Test
Use [Hibernate](https://tapestry.apache.org/using-tapestry-with-hibernate.html) for interaction with MySQL database.

* [Database test source](hello/src/main/java/hello/pages/HelloDB.java)
* [Database Entity](hello/src/main/java/hello/entities/World.java)
* [Hibernate Configuration](hello/src/main/resources/hibernate.cfg.xml)

## Versions

* [Java OpenJDK 1.7.0_09](http://openjdk.java.net/)
* [Tapestry 5.3.6](http://tapestry.apache.org)
* [Hibernate 3.6.3](http://www.hibernate.org/)
* [MySQL 5.5.29](http://dev.mysql.com/)
* [Maven 2.2.1](https://maven.apache.org/)
* [Resin 4.0.34](http://www.caucho.com/)

## Test URLs

### JSON Encoding Test

    http://localhost:8080/tapestry/hellojson

### Database Mapping Test

    http://localhost:8080/tapestry/hellodb?queries=5
