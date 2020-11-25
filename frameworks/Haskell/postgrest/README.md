# PostgREST Benchmarking Test

### Test Type Implementation Source Code

* [JSON](src/json.sql)
* [PLAINTEXT](src/plaintext.sql)
* [DB](src/db.sql)
* [QUERY](src/query.sql)
* [CACHED QUERY](Relative/Path/To/Your/Source/File)
* [UPDATE](Relative/Path/To/Your/Source/File)
* [FORTUNES](src/fortunes.sql)

## Important Libraries
The tests were run with:
* docker-compose down && docker-compose build && docker-compose up

## Test URLs
### JSON

http://localhost:3000/rpc/json

### PLAINTEXT

http://localhost:3000/rpc/plaintext

### DB

http://localhost:3000/rpc/db

### QUERY

http://localhost:3000/rpc/query?queries=

### CACHED QUERY

http://localhost:8080/cached_query?queries=

### UPDATE

http://localhost:3000/rpc/update?queries=

### FORTUNES

http://localhost:3000/rpc/fortunes.html
