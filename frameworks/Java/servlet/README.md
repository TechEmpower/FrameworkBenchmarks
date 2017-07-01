# Servlet Benchmarking Test

This is the Java Servlet portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### Plaintext and JSON

* [Plaintext test source](src/main/java/hello/PlaintextServlet.java)
* [JSON test source](src/main/java/hello/JsonServlet.java)

### `MySQL` implementation

* [DB test source](src/main/java/hello/DbPoolServlet.java)
* [Queries test source](src/main/java/hello/DbPoolServlet.java)
* [Updates test source](src/main/java/hello/UpdateServlet.java) - using `batch updates`
* [Fortune test source](src/main/java/hello/FortunesServlet.java)

### `PostgreSQL` implementation

DB, Queries and Fortune use the same implementation as MySQL.

* [Updates test source](src/main/java/hello/PostgresUpdateServlet.java) - **not** using `batch updates` due to transaction deadlocks
* [Cache test source](src/main/java/hello/Cache2kPostgresServlet.java)

## Infrastructure Software Versions

The tests were run with:

* [Java Oracle JDK 1.8.0](http://openjdk.java.net/)
* [Resin 4.0.53](http://www.caucho.com/)
* [Jackson 2.8.9](http://wiki.fasterxml.com/JacksonHome)
* [MySQL 5.7](https://dev.mysql.com/)
* [Postgres 9.3](http://www.postgresql.org/)
* [cache2k 1.0.0.CR4](https://cache2k.org/)

Please confirm the versions data with the latest install scripts of TFB project.

## Test URLs

### Default maven profile

 * Plaintext - `http://localhost:8080/servlet/plaintext`
 * JSON - `http://localhost:8080/servlet/json`

### `mysql` and `postgresql` Maven profiles

 * DB - `http://localhost:8080/servlet/db`
 * Queries - `http://localhost:8080/servlet/db?queries=`
 * Updates - `http://localhost:8080/servlet/update?queries=`
 * Fortune - `http://localhost:8080/servlet/fortunes`

### `postgresql` Maven profile
 
 * Cache - `http://localhost:8080/servlet/cached-worlds`