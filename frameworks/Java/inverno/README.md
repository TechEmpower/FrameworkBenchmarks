# inverno Benchmarking Test

### Test Type Implementation Source Code

* [JSON](src/main/java/com/techempower/inverno/benchmark/internal/Controller.java)
* [PLAINTEXT](src/main/java/com/techempower/inverno/benchmark/internal/Controller.java)
* [DB](src/main/java/com/techempower/inverno/benchmark/internal/Controller.java)
* [QUERY](src/main/java/com/techempower/inverno/benchmark/internal/Controller.java)
* [CACHED QUERY](src/main/java/com/techempower/inverno/benchmark/internal/Controller.java)
* [UPDATE](src/main/java/com/techempower/inverno/benchmark/internal/Controller.java)
* [FORTUNES](src/main/java/com/techempower/inverno/benchmark/internal/Controller.java)

## Important Libraries

The tests were run with:
* [Java OpenJDK 21](https://openjdk.java.net/)
* [Inverno 1.12.0](https://inverno.io)
* [DSL-JSON 2.0.2](https://github.com/ngs-doo/dsl-json)

## Test URLs

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
