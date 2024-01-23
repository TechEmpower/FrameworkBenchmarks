# Lwan Benchmarking Test

This test is based on of the [Lwan](https://lwan.ws) web-server project,
an experimental high-performance web server.

# C version

## Source code

* [JSON](src/techempower.c)
* [Plain text](src/techempower.c)
* [DB](src/techempower.c)
* [Query](src/techempower.c)
* [Cached Queries](src/techempower.c)
* [Fortunes](src/techempower.c)

## Test URLs
### JSON

http://localhost:8080/json

### Plaintext

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### Query

http://localhost:8080/query?queries=

### Cached Queries

http://localhost:8080/cached-queries?count=

### Fortunes

http://localhost:8080/fortunes

# Lua version

## Source code

* [JSON](src/techempower.conf)
* [Plaintext](src/techempower.conf)

## Test URLs
### JSON

http://localhost:8080/lua.json

### Plaintext

http://localhost:8080/lua.plaintext

