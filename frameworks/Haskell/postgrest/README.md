# PostgREST Benchmarking Test

### Test Type Implementation Source Code

* [JSON](src/json.sql)
* [PLAINTEXT](src/plaintext.sql)
* [DB](src/db.sql)
* [QUERY](src/query.sql)
* [CACHED QUERY] Not Implemented
* [UPDATE](src/update.sql)
* [FORTUNES](src/fortunes.sql)

## Important Libraries
The tests were run with:
* docker-compose down && docker-compose build && docker-compose up

## Test URLs
### JSON

http://localhost:3000/rpc/jsonser

### PLAINTEXT

http://localhost:3000/rpc/plaintext

### DB

http://localhost:3000/rpc/db

### QUERY

http://localhost:3000/rpc/query?queries=

### CACHED QUERY - Not Implemented

http://localhost:8080/cached_query?queries=

### UPDATE - Not Working

http://localhost:3000/rpc/updates?queries=

Technically, this is implemented (maybe not correctly though).
However, the benchmark issues this as a GET request.
PostgREST sets the transaction to READ ONLY for GET requests,
as they are supposed to be idempotent.
Hence this results in an error. Calling the endpoint with POST
works though.

### FORTUNES

http://localhost:3000/rpc/fortunes.html
