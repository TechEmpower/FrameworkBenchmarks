# inverno Benchmarking Test

### Test Type Implementation Source Code

* [JSON](src/main/java/com/techempower/inverno/benchmark/internal/Handler.java)
* [PLAINTEXT](src/main/java/com/techempower/inverno/benchmark/internal/Handler.java)
* [DB](src/main/java/com/techempower/inverno/benchmark/internal/Handler.java)
* [QUERY](src/main/java/com/techempower/inverno/benchmark/internal/Handler.java)
* [CACHED QUERY](src/main/java/com/techempower/inverno/benchmark/internal/Handler.java)
* [UPDATE](src/main/java/com/techempower/inverno/benchmark/internal/Handler.java)
* [FORTUNES](src/main/java/com/techempower/inverno/benchmark/internal/Handler.java)

## Important Libraries
The tests were run with:
* [Java OpenJDK 16](https://openjdk.java.net/)
* [Inverno 1.2.1](https://inverno.io)

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
