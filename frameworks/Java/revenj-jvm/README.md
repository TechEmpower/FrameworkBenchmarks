# Revenj.JVM Benchmarking Test

Servlet + Revenj DB API + DSL-JSON + Rocker + PostgreSQL.
It uses precompiled DSL model for POJO classes.
It uses Rocker for HTML output

### DSL model
Data structures are defined in a DSL schema

 * [DSL source](src/main/java/hello/model.dsl)

### Test sources

 * [Plaintext](src/main/java/hello/PlaintextServlet.java)
 * [JSON](src/main/java/hello/JsonServlet.java)
 * [DB](src/main/java/hello/DbServlet.java)
 * [Queries](src/main/java/hello/QueriesServlet.java)
 * [Updates](src/main/java/hello/UpdatesServlet.java)
 * [Fortune](src/main/java/hello/FortunesServlet.java)

## Software Versions
The tests were run with:

 * [Oracle Java 1.8](https://www.oracle.com/java/)
 * [Postgres 9.3](http://www.postgresql.org/)
 * [Resin 4.0](http://www.caucho.com/)
 * [DSL JSON 1.4.0](http://github.com/ngs-doo/dsl-json)
 * [Revenj.Java 1.2.0](http://github.com/ngs-doo/revenj)
 * [Rocker 0.2.0](https://github.com/fizzed/rocker)

## Test URLs

 * Plaintext - `http://localhost:8080/revenj/plaintext`
 * JSON - `http://localhost:8080/revenj/json`
 * DB - `http://localhost:8080/revenj/db`
 * Queries - `http://localhost:8080/revenj/queries?queries={count}`
 * Updates -  `http://localhost:8080/revenj/updates?queries={count}`
 * Fortune -  `http://localhost:8080/revenj/fortunes`
