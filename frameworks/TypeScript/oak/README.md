# oak Benchmarking Test

### Test Type Implementation Source Code

- [PLAINTEXT](src/routes.ts#L18)
- [JSON](src/routes.ts#L19)
- [DB](src/routes.ts#L25)
- [QUERY](src/routes.ts#L29)
- [UPDATE](src/routes.ts#L39)
- [FORTUNES](src/routes.ts#L59)
- [CACHED QUERY](src/routes.ts#L73)

## Important Libraries

The tests were run with:

- [deno](https://deno.land)
- [oak](https://deno.land/x/oak/)
- [Postgres.js](https://github.com/porsager/postgres/)

## Test URLs

### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/queries?q=

### UPDATE

http://localhost:8080/updates?q=

### FORTUNES

http://localhost:8080/fortunes

### CACHED QUERY

http://localhost:8080/cached_queries?q=