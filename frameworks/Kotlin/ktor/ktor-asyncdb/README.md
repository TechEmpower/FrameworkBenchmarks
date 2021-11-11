# Ktor with async postgres Benchmarking Test

## Important Libraries
This sets up testing using [Ktor](https://ktor.io/), with a couple of async PostgreSQL clients:
* [jasync-sql](https://github.com/jasync-sql/jasync-sql) A pure Kotlin MySQl/PostgreSQL client, with coroutines integration
* [reactive-pg-client](https://reactiverse.io/reactive-pg-client/guide/java/) A mature async PostgreSQL client with pipelining and batch support

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes