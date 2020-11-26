# RestExpress Benchmarking Test

[RestExpress](http://www.restexpress.org/) - composes tools to form a lightweight, minimalist Java framework for quickly creating RESTful microservices.

Test implementations with MongoDB and MySQL as a backing datastore. MongoDB is using Morphia framework for object persistance.

### Test sources

Which `Controllers` will be instantiated and wired is determined from the existense of the MongoDB or MySQL connection string.

**Important** 
 * The build is using a 0.12.0-SNAPSHOT version of the RestExpress. There were problems at startup with the bundled Netty in the 0.11.3 version;
 * The MongoDB repository class is copy-pasted and new argument is introduced to get around the mandatory initialization of MongoDB indexes and collection caps.

### MongoDB
 * [Plaintext](src/main/java/hello/controller/PlaintextController.java)
 * [JSON](src/main/java/hello/controller/JsonController.java)
 * [DB](src/main/java/hello/controller/MongodbController.java)
 * [Queries](src/main/java/hello/controller/QueriesMongodbController.java)

### MySQL

 * [DB](src/main/java/hello/controller/MysqlController.java)
 * [Queries](src/main/java/hello/controller/QueriesMysqlController.java)

## Software Versions

The tests were run with:

 * [Oracle Java 10](https://www.oracle.com/java/)
 * [MySQL 5.7](http://www.mysql.com/)
 * [MongoDB](http://www.mongodb.com/)
 * [Morphia](https://morphia.dev/)

Please check the versions in the install and build scripts of TFB project.

## Test URLs

All implementations use the same URLs.

 * Plaintext - `http://localhost:8080/plaintext`
 * JSON - `http://localhost:8080/json`
 * DB - `http://localhost:8080/db`
 * Queries - `http://localhost:8080/query?queries=`
