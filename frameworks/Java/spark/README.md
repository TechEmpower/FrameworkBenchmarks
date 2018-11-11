# Spark Benchmarking Test

This is the Spark portion of a [benchmarking test suite](../) comparing a variety of web development platforms.
The test utilizes Spark routes, Gson for JSON serialization, Hibernate for ORM and mustache templates rendering.
HikariCP is used for connection pooling, using up to 2*cores count. See [pool sizing](https://github.com/brettwooldridge/HikariCP/wiki/About-Pool-Sizing)


## Tests

* [Spark application](/src/main/java/hello/web/SparkApplication.java)
* [Hibernate](http://www.hibernate.org/) configuration for local datasource
 * [Local datasource configuration](/src/main/resources/hibernate-local.cfg.xml)
 * [Hibernate utilities](/src/main/java/hello/web/HibernateUtil.java)
 * [Database entity](/src/main/java/hello/domain/World.java)


## Infrastructure Software Versions

* [Spark 2.7.1](http://www.sparkjava.com/)
* [Hibernate 5.3.6.Final](http://www.hibernate.org/)
* [Gson 2.8.5](https://github.com/google/gson)


## Different test setups

* Local environment with Spark's built in embedded jetty (port=8080, context=/)
 * Start application from [SparkApplication](/src/main/java/hello/web/SparkApplication.java)'s main method

## Test URLs

### JSON Encoding Test

http://localhost:8080/json

### Database Test

http://localhost:8080/db

### Query Test

http://localhost:8080/db?queries=5

### Update Test

http://localhost:8080/updates?queries=5

### Fortune cookie Test

http://localhost:8080/fortunes

### Plain Text Test

http://localhost:8080/plaintext