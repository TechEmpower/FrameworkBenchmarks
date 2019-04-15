# Eclipse Grizzly Benchmarking Test

[Eclipse Grizzly](https://projects.eclipse.org/projects/ee4j.grizzly) - HTTP Protocol framework and HTTP server framework.

### Test sources

Currently implemented tests are:

 * [Plaintext](src/main/java/org/glassfish/grizzly/bm/PlainText2HttpHandler.java)
 * [JSON](src/main/java/org/glassfish/grizzly/bm/JsonHttpHandler.java)

The [Server.java](src/main/java/org/glassfish/grizzly/bm/Server.java) is modifying the standard configuration a lot. It is utilizing the ["Same thread I/O strategy"](https://javaee.github.io/grizzly/iostrategies.html). Note: this needs confirmation!

Also the command line arguments are modified: `java -Dorg.glassfish.grizzly.nio.transport.TCPNIOTransport.max-receive-buffer-size=16384 -Dorg.glassfish.grizzly.http.io.OutputBuffer.default-buffer-size=1024 -Dorg.glassfish.grizzly.memory.BuffersBuffer.bb-cache-size=32 -jar app.jar`

## Software Versions

The tests were run with:

 * [Oracle Java 10](https://www.oracle.com/java/)
 * [Jackson](https://github.com/FasterXML/jackson/wiki/Jackson-Releases)

Please check the versions in the install and build scripts of TFB project.

## Test URLs

 * Plaintext - `http://localhost:8080/plaintext`
 * JSON - `http://localhost:8080/json`


# Eclipse Jersey with Grizzly Benchmarking Test

[Eclipse Jersey](https://projects.eclipse.org/projects/ee4j.jersey) - REST framework that provides a JAX-RS (JSR-370) implementation and more.

### Test sources

Currently implemented tests are:

 * [Plaintext](src/main/java/org/glassfish/grizzly/bm/PlainText2HttpHandler.java)
 * [JSON](src/main/java/org/glassfish/grizzly/bm/JsonHttpHandler.java)
 * [DB test source](src/main/java/hello/WorldResource.java)
 * [Queries test source](src/main/java/hello/WorldResource.java)
 * [Updates test source](src/main/java/hello/WorldResource.java)
 * [Fortune test source](src/main/java/hello/FortunesResource.java)

 The [WebServer.java](src/main/java/hello/WebServer.java) is modifying the standard configuration. It is made to mimic the configuration in the Grizzly benchmark implementation.

## Software Versions

The tests were run with:

 * [Oracle Java 10](https://www.oracle.com/java/)
 * [PostgreSQL](http://www.postgresql.org/)
 * [Eclipse Grizzly](https://projects.eclipse.org/projects/ee4j.grizzly)
 * [Jackson](https://github.com/FasterXML/jackson/wiki/Jackson-Releases)
 * [Hibernate ORM](https://hibernate.org/orm/)
 * [mustache.java](https://github.com/spullara/mustache.java)

Please check the versions in the install and build scripts of TFB project.

## Test URLs

 * Plaintext - `http://localhost:8080/plaintext`
 * JSON - `http://localhost:8080/json`
 * DB - `http://localhost:8080/db?single=true`
 * Queries - `http://localhost:8080/db?queries=`
 * Updates - `http://localhost:8080/update?queries=`
 * Fortune - `http://localhost:8080/fortunes`