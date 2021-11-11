# [Micronaut](http://micronaut.io) Benchmarking Test (3.1.1)

### Test Type Implementation Source Code

* [JSON](src/main/java/benchmark/JsonSerialization.java)
* [PLAINTEXT](src/main/java/benchmark/PlainText.java)
* [DB](src/main/java/benchmark/Database.java)
* [QUERY](src/main/java/benchmark/Database.java)
* [UPDATE](src/main/java/benchmark/Database.java)
* [FORTUNES](src/main/java/benchmark/Database.java)

## Important Libraries
The tests were run with:
* [OpenJDK Java 11](http://jdk.java.net/11/)
* [GraalVM CE 21.3.0 (Java 11)](https://www.graalvm.org)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/queries?queries=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes
