# Servlet Benchmarking Test

This is the Java Servlet portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

There is a Servlet 3.1 based tests implementation in the [servlet3](./servlet3) sub-folder. It is using Tomcat 9 as Servlet containter.

There are two retired implementations for the JSON test: usage of Jackson Afterburner module and usage of a custom Jackson serialization.

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

* [Java Oracle JDK](http://openjdk.java.net/)
* [Resin](http://www.caucho.com/)
* [Jackson](http://wiki.fasterxml.com/JacksonHome)
* [MySQL](https://dev.mysql.com/)
* [Postgres](http://www.postgresql.org/)
* [cache2k](https://cache2k.org/)

Please confirm the versions data with the latest install scripts of TFB project.

## Test URLs

### Default Maven profile

 * Plaintext - `http://localhost:8080/plaintext`
 * JSON - `http://localhost:8080/json`

### `mysql` and `postgresql` Maven profiles

 * DB - `http://localhost:8080/db`
 * Queries - `http://localhost:8080/db?queries=`
 * Updates - `http://localhost:8080/update?queries=`
 * Fortune - `http://localhost:8080/fortunes`

### `postgresql` Maven profile
 
 * Cache - `http://localhost:8080/cached-worlds`
