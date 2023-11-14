# Pippo Benchmarking Test


### Test Type Implementation Source Code

* [JSON](src/main/java/com/techempower/benchmark/pippo/handler/Test1JsonHandler.java)
* [PLAINTEXT](src/main/java/com/techempower/benchmark/pippo/handler/Test6PlainTextHandler.java)
* [DB](src/main/java/com/techempower/benchmark/pippo/handler/Test2SingleQueryHandler.java)
* [QUERY](src/main/java/com/techempower/benchmark/pippo/handler/Test3MultiQueryHandler.java)
* [CACHED QUERY](-)
* [UPDATE](src/main/java/com/techempower/benchmark/pippo/handler/Test5UpdateHandler.java)
* [FORTUNES](src/main/java/com/techempower/benchmark/pippo/handler/Test4FortuneHandler.java)

## Important Libraries

The tests were run with:

* [Java 17](https://openjdk.java.net)
* [Pippo 1.14.0](http://www.pippo.ro/)
* [HikariCP 5.0.1](https://github.com/brettwooldridge/HikariCP)
* [DSL-JSON 1.10.0](https://github.com/ngs-doo/dsl-json)

## Test URLs

### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/postgres/db
http://localhost:8080/mysql/db
http://localhost:8080/mongo/db

### QUERY

http://localhost:8080/postgres/queries?queries=
http://localhost:8080/mysql/queries?queries=
http://localhost:8080/mongo/queries?queries=

### CACHED QUERY

-

### UPDATE

http://localhost:8080/postgres/updates?queries=
http://localhost:8080/mysql/updates?queries=
http://localhost:8080/mongo/updates?queries=

### FORTUNES

http://localhost:8080/postgres/fortunes
http://localhost:8080/mysql/fortunes
http://localhost:8080/mongo/fortunes
