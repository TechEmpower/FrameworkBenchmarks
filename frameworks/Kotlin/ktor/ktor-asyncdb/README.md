# Ktor with async postgres Benchmarking Test

## Important Libraries
This sets up testing using [Ktor](https://ktor.io/), with a couple of async PostgreSQL clients:
* [jasync-sql](https://github.com/jasync-sql/jasync-sql) A pure Kotlin MySQl/PostgreSQL client, with coroutines integration
* [reactive-pg-client](https://reactiverse.io/reactive-pg-client/guide/java/) A mature async PostgreSQL client with pipelining and batch support

## Test URLs
### JSON

http://localhost:9090/json

### PLAINTEXT

http://localhost:9090/plaintext

### DB

http://localhost:9090/db

### QUERY

http://localhost:9090/query?queries=

### UPDATE

http://localhost:9090/update?queries=

### FORTUNES

http://localhost:9090/fortunes