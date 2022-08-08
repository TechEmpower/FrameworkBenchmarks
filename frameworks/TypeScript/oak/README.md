# oak Benchmarking Test

### Test Type Implementation Source Code

- [PLAINTEXT](src/routes.ts#L15)
- [JSON](src/routes.ts#L16)
- [DB](src/routes.ts#L17)
- [QUERY](src/routes.ts#L24)
- [UPDATE](src/routes.ts#L37)
- [FORTUNES](src/routes.ts#L53)
- [CACHED QUERY](src/routes.ts#L67)

## Important Libraries

The tests were run with:

- [deno](https://deno.land)
- [oak](https://deno.land/x/oak/)

## Test URLs

### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?q=

### UPDATE

http://localhost:8080/update?q=

### FORTUNES

http://localhost:8080/fortunes

### CACHED QUERY

http://localhost:8080/cached_query?q=