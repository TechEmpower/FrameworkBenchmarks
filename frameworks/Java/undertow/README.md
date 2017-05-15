# Undertow

This is the test for the Undertow web server.

* [Project website](http://undertow.io/)
* [GitHub repository](https://github.com/undertow-io/undertow)

## Test types

This implements all benchmark test types.  The database tests are implemented
for MySQL, PostgreSQL, and MongoDB databases.

[HelloWebServer.java](src/main/java/hello/HelloWebServer.java) is the entry
point for the application, providing the `main` method.

**The only test type that exercises Undertow in isolation is the plaintext
test**.  For functionality that Undertow does not provide &mdash; JSON encoding,
database connectivity &mdash; this implementation depends on popular third party
libraries that are expected to perform well.  We hope for these tests to serve
as performance baselines for benchmarks of other frameworks that are implemented
on top of Undertow.

### Plaintext

URL: `http://TFB-server:8080/plaintext`

Source code:
* [PlaintextHandler.java](src/main/java/hello/PlaintextHandler.java)

Additional libraries used: (None)

### JSON

URL: `http://TFB-server:8080/json`

Source code:
* [JsonHandler.java](src/main/java/hello/JsonHandler.java)

Additional libraries used:
* [Jackson]

### Database single-query

URL: `http://TFB-server:8080/db`

Source code:
* [DbSqlHandler.java](src/main/java/hello/DbSqlHandler.java)
* [DbMongoHandler.java](src/main/java/hello/DbMongoHandler.java)
* [DbMongoAsyncHandler.java](src/main/java/hello/DbMongoAsyncHandler.java)

Additional libraries used:
* [Jackson]
* [MySQL Connector/J]
* [PostgreSQL JDBC Driver]
* [Java MongoDB Driver]
* [HikariCP]

### Database multi-query

URL: `http://TFB-server:8080/queries?queries={integer}`

Source code:
* [QueriesSqlHandler.java](src/main/java/hello/QueriesSqlHandler.java)
* [QueriesMongoHandler.java](src/main/java/hello/QueriesMongoHandler.java)
* [QueriesMongoAsyncHandler.java](src/main/java/hello/QueriesMongoAsyncHandler.java)

Additional libraries used:
* [Jackson]
* [MySQL Connector/J]
* [PostgreSQL JDBC Driver]
* [Java MongoDB Driver]
* [HikariCP]

### Database updates

URL: `http://TFB-server:8080/updates?queries={integer}`

Source code:
* [UpdatesSqlHandler.java](src/main/java/hello/UpdatesSqlHandler.java)
* [UpdatesMongoHandler.java](src/main/java/hello/UpdatesMongoHandler.java)
* [UpdatesMongoAsyncHandler.java](src/main/java/hello/UpdatesMongoAsyncHandler.java)

Additional libraries used:
* [Jackson]
* [MySQL Connector/J]
* [PostgreSQL JDBC Driver]
* [Java MongoDB Driver]
* [HikariCP]

### Fortunes

URL: `http://TFB-server:8080/fortunes`

Source code:
* [FortunesSqlHandler.java](src/main/java/hello/FortunesSqlHandler.java)
* [FortunesMongoHandler.java](src/main/java/hello/FortunesMongoHandler.java)
* [FortunesMongoAsyncHandler.java](src/main/java/hello/FortunesMongoAsyncHandler.java)

Additional libraries used:
* [Mustache.java]
* [MySQL Connector/J]
* [PostgreSQL JDBC Driver]
* [Java MongoDB Driver]
* [HikariCP]

[Jackson]: https://github.com/FasterXML/Jackson
[Mustache.java]: https://github.com/spullara/mustache.java
[MySQL Connector/J]: https://dev.mysql.com/downloads/connector/j/5.1.html
[PostgreSQL JDBC Driver]: https://jdbc.postgresql.org/
[Java MongoDB Driver]: https://docs.mongodb.com/ecosystem/drivers/java/
[HikariCP]: https://github.com/brettwooldridge/HikariCP
