# Eclipse Grizzly Benchmarking Test

[Eclipse Grizzly](https://projects.eclipse.org/projects/ee4j.grizzly) - HTTP Protocol framework and HTTP server framework.

### Test sources

Currently implemented tests are:

 * [Plaintext](src/main/java/org/glassfish/grizzly/bm/PlainText2HttpHandler.java)
 * [JSON](src/main/java/org/glassfish/grizzly/bm/JsonHttpHandler.java)

 The [Server.java](src/main/java/org/glassfish/grizzly/bm/Server.java) is modifying the standard configuration a lot. It is utilizing the ["Same thread I/O strategy"](https://javaee.github.io/grizzly/iostrategies.html). Note: this needs confirmation!

## Software Versions

The tests were run with:

 * [Oracle Java 10](https://www.oracle.com/java/)
 * [Jackson](https://github.com/FasterXML/jackson/wiki/Jackson-Releases)

Please check the versions in the install and build scripts of TFB project.

## Test URLs

 * Plaintext - `http://localhost:8080/plaintext`
 * JSON - `http://localhost:8080/json`
