# Dropwizard Benchmarking Test

[Dropwizard](http://dropwizard.io/) - ops-friendly, high-performance, RESTful web services.

Mustashe tempate is used for the HTML tests.
Test implementations with MongoDB, MySQL and PostgreSQL for backing datastore.
Test implementations with Hibernate and JDBI as ORM frameworks, MongoJack for POJO to MongoDB document mapping.

**Note**: The [Dropwizard Metrics](http://metrics.dropwizard.io/) are enabled and gathering data during the test runs.

### Test sources

All test implementations share the same API end-points and POJO model classes. The API end-points are called `resources` in Dropwizard. This is the list:

 * [Plaintext](src/main/java/com/example/helloworld/resources/TextResource.java)
 * [JSON](src/main/java/com/example/helloworld/resources/JsonResource.java)
 * [DB](src/main/java/com/example/helloworld/resources/WorldResource.java)
 * [Queries](src/main/java/com/example/helloworld/resources/WorldResource.java)
 * [Updates](src/main/java/com/example/helloworld/resources/WorldResource.java)
 * [Fortune](src/main/java/com/example/helloworld/resources/FortuneResource.java)

## Source code organisation

| Test implementation | Dropwizard Service class | Dropwizard Configuration class| Maven profile |
| --- | --- | --- | --- |
| MongoDB&MongoJack | HelloMongoService.java | HelloMongoConfiguration.java | `-P mongo` |
| MySQL&Hibernate | HelloWorldService.java | HelloWorldConfiguration.java | `-P mysql` |
| PostgreSQL&Hibernate | HelloWorldService.java | HelloWorldConfiguration.java | `-P postgres` |
| PostgreSQL&JDBI | HelloJDBIService.java | HelloWorldConfiguration.java | `-P postgres,jdbi` |

Each implementation has its own set of `.yml` configuration file and shell script.

### MongoDB

 * [DB, Queries and Updates](src/main/java/com/example/helloworld/db/mongo/WorldMongoImpl.java)
 * [Fortune](src/main/java/com/example/helloworld/db/mongo/FortuneMongoImpl.java)

### (MySQL or PostgreSQL) and Hibernate

The `Updates` test is using JDBC batch updates and manual transaction handling.

 * [DB, Queries and Updates](src/main/java/com/example/helloworld/db/hibernate/WorldHibernateImpl.java)
 * [Fortune](src/main/java/com/example/helloworld/db/hibernate/FortuneHibernateImpl.java)

### PostgeSQL and JDBI

 * [DB, Queries and Updates](src/main/java/com/example/helloworld/db/jdbi/WorldRepository.java)
 * [Fortune](src/main/java/com/example/helloworld/db/jdbi/FortuneJDBIImpl.java)

## Software Versions

The tests were run with:

 * [Oracle Java 1.8](https://www.oracle.com/java/)
 * [Postgres 9.3](http://www.postgresql.org/)
 * [MySQL 5.7](http://www.mysql.com/)
 * [MongoDB 3.2](http://www.mongodb.com/)
 * [Dropwizard](http://dropwizard.io/)
 * [MongoJack](http://mongojack.org/)

Please check the versions in the install scripts of TFB project.

## Test URLs

All implementations use the same URLs.

 * Plaintext - `http://localhost:9090/plaintext`
 * JSON - `http://localhost:9090/json`
 * DB - `http://localhost:9090/db`
 * Queries - `http://localhost:9090/db?queries=`
 * Updates - `http://localhost:9090/db/update?queries=?`
 * Fortune - `http://localhost:9090/fortunes`
