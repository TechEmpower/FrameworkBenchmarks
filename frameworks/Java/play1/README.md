# Play 1 framework Benchmarking Test

[Play 1 framework](https://www.playframework.com/) - the high velocity web framework for Java and Scala. This is the old version - it's not Play 2.

### Test sources

This is the list:

 * [Plaintext](app/controllers/Application.java#L24)
 * [JSON](app/controllers/Application.java#L28)
 * [DB](app/controllers/Application.java#L39)
 * [Queries](app/controllers/Application.java#L45)

## Software Versions

The tests were run with:

 * [Oracle Java 10](https://www.oracle.com/java/)
 * [MySQL 5.7](http://www.mysql.com/)

Please check the versions in the install and build scripts of TFB project.

## Test URLs

All implementations use the same URLs.

 * Plaintext - `http://localhost:8080/plaintext`
 * JSON - `http://localhost:8080/json`
 * DB - `http://localhost:8080/db`
 * Queries - `http://localhost:8080/query?queries=`

