# Javalin Benchmarking Test

### Test Type Implementation Source Code

* [All Tests](src/main/java/benchmark/Main.java)

## Important Libraries
The tests were run with:
* [Java 11](https://openjdk.java.net)
* [Javalin 5.0.1](https://github.com/tipsy/javalin)
* [HikariCP 5.0.1](https://github.com/brettwooldridge/HikariCP)
* [jte 2.2.1](https://github.com/casid/jte)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

http://localhost:8080/mongodb/db

### QUERY

http://localhost:8080/queries?queries=

http://localhost:8080/mongodb/queries?queries=

### UPDATE

http://localhost:8080/updates?queries=

http://localhost:8080/mongodb/updates?queries=

### FORTUNES

http://localhost:8080/fortunes

http://localhost:8080/mongodb/fortunes
