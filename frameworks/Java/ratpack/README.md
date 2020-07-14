# Ratpack Benchmarking Test

This is the Ratpack portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

Ratpack's [hikari module](https://github.com/ratpack/ratpack/tree/master/ratpack-hikari) is used to managed the connection pool. It is configured for a maximum of (2 * cores count) concurrent connections. See [About-Pool-Sizing](https://github.com/brettwooldridge/HikariCP/wiki/About-Pool-Sizing) for more information. I am assuming that the DB is running on a computer with the same spec as the one running the app, which seems to be the case based on what is written [here](https://www.techempower.com/benchmarks/#section=environment&hw=ph&test=db&l=fjd9b3)

Ratpack's [handlebars module](https://github.com/ratpack/ratpack/tree/master/ratpack-handlebars) is used to render the fortune template.

There are two repository implementations.
* [JdbcRepository](src/main/java/models/JdbcRepository.java) is using JDBC and an unbounded thread pool to prevent blocking the main event loop.
* [PgClientRepository](src/main/java/models/PgClientRepository.java) is using an asynchronous driver to query the database.

### Plaintext Test

* [Plaintext test source](src/main/java/handlers/PlainTextHandler.java)

### JSON Serialization Test

* [JSON test source](src/main/java/handlers/JsonHandler.java)

### Database Query Test

* [Database Query test source](src/main/java/handlers/DbHandler.java)

### Database Queries Test

* [Database Queries test source](src/main/java/handlers/QueryHandler.java)

### Database Update Test

* [Database Update test source](src/main/java/handlers/UpdateHandler.java)

### Template rendering Test

* [Template rendering test source](src/main/java/handlers/FortuneHandler.java)

## Versions

* [Java OpenJDK 11](http://openjdk.java.net/)
* [Ratpack 1.7.6](http://ratpack.io/)
* [reactive-pg-client 0.11.4](https://github.com/reactiverse/reactive-pg-client)

## Test URLs

### Plaintext Test

    http://localhost:5050/plaintext

### JSON Encoding Test

    http://localhost:5050/json

### Database Query Test

    http://localhost:5050/db

### Database Queries Test

    http://localhost:5050/queries?queries=5

### Database Update Test

    http://localhost:5050/updates?queries=5

### Template rendering Test

    http://localhost:5050/fortunes
