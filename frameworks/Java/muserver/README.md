# muserver Benchmarking Test

### Test Type Implementation Source Code

* [JSON](src/main/java/benchmark/TFBBase.java)
* [PLAINTEXT](src/main/java/benchmark/TFBBase.java)
* [DB](src/main/java/benchmark/TFBBase.java)
* [QUERY](src/main/java/benchmark/TFBBase.java)
* [UPDATE](src/main/java/benchmark/TFBBase.java)
* [FORTUNES](src/main/java/benchmark/TFBBase.java)

## Important Libraries

The tests were run with:

* [Java 21](https://jdk.java.net/21/)
* [muserver 2.1.10](https://muserver.io/)
* [Jackson 2.19.0](https://github.com/FasterXML/jackson)
* [Pebble 3.2.4](https://pebbletemplates.io/)
* [Postgres JDBC Driver 42.7.5](https://jdbc.postgresql.org/)
* [HikariCP 6.3.0](https://github.com/brettwooldridge/HikariCP)

## Test URLs

For running default test (TFBRest.java), please append "/rest" to the path.

### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=

### CACHED QUERY

http://localhost:8080/cached_query?queries=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes


## Reference

The Loom support and IO_URING support are modified from [netty test](https://github.com/TechEmpower/FrameworkBenchmarks/tree/master/frameworks/Java/netty).

The database connection part is modified from [Javalin test](https://github.com/TechEmpower/FrameworkBenchmarks/tree/master/frameworks/Java/javalin).