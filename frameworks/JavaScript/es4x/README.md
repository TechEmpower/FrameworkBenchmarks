# ES4X Benchmarking Test

### Test Type Implementation Source Code

* [JSON](index.js#L24-L30)
* [PLAINTEXT](index.js#L233-L239)
* [DB](index.js#L50-L71)
* [QUERY](index.js#L78-L108)
* [UPDATE](index.js#L171-L225)
* [FORTUNES](index.js#L114-L164)

## Important Libraries
The tests were run with:
* [Eclipse Vert.x](https://vertx.io/)
* [GraalVM](https://www.graalvm.org/)
* [ES4X](https://reactiverse.io/es4x/)

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

http://localhost:8080/updates?queries=

### FORTUNES

http://localhost:8080/fortunes
