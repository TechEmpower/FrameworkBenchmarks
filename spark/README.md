# Spark Benchmarking Test

This is the Spark portion of a [benchmarking test suite](../) comparing a variety of web development platforms.
The test utilizes Spark routes, Gson for JSON serialization, Hibernate for ORM and a custom OSIV pattern created
with Spark filters.


## Tests

* [Spark application](/src/main/java/hello/web/SparkApplication.java)
* [Hibernate](http://www.hibernate.org/) configuration for local datasource and container managed JNDI
 * [JNDI configuration](/world/src/main/resources/hibernate-jndi.cfg.xml)
 * [Local datasource configuration](/world/src/main/resources/hibernate-local.cfg.xml)
 * [Hibernate utilities](/src/main/java/hello/web/HibernateUtil.java)
 * [Database entity](/src/main/java/hello/domain/World.java)


## Infrastructure Software Versions

* [Spark 0.9.9.7-SNAPSHOT](http://www.sparkjava.com/)
* [Hibernate 4.2.0.Final](http://www.hibernate.org/)
* [Gson 2.2.2](https://code.google.com/p/google-gson/)


## Different test setups

* Local environment with Spark's built in embedded jetty (port=4567, context=/)
 * Start application from [SparkApplication](/world/src/main/java/hello/web/SparkApplication.java)'s main method
 * 'standalone' maven profile must be enabled from [pom.xml](/pom.xml)
* Local environment with Tomcat maven plugin (port=8080, context=/spark)
 * Start application with maven command 'mvn clean tomcat7:run'
 * No maven profiles must be enabled
* Any servlet container with built WAR (port=any, context=/spark)
 * Create war with maven command 'mvn clean package'
 * No maven profiles must be enabled
 * Built war can be copied from /target/spark.war

* Local datasource or JNDI datasource can be configured with system property 'jndi'
 * -Djndi=true or no property for JNDI datasource
 * -Djndi=false for local datasource

## Test URLs

### JSON Encoding Test

http://localhost:4567/json

http://localhost:8080/spark/json

### Data-Store/Database Mapping Test

http://localhost:4567/db?queries=5

http://localhost:8080/spring/db?queries=5
