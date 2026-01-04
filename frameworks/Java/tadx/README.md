# Tad.x (tadx) Benchmarking Test

### Test Type Implementation Source Code

* [JSON](src/main/java/io/tadx/benchmark/route_mapper/JsonRouteMapper.java)
* [PLAINTEXT](src/main/java/io/tadx/benchmark/route_mapper/PlainTextRouteMapper.java)
* [DB](src/main/java/io/tadx/benchmark/route_mapper/DbRouteMapper_Postgresql.java)
* [QUERY](src/main/java/io/tadx/benchmark/route_mapper/QueriesRouteMapper1_Postgresql.java)
* [CACHED QUERY](src/main/java/io/tadx/benchmark/route_mapper/CachedQueriesMapper3.java)
* [UPDATE](src/main/java/io/tadx/benchmark/route_mapper/UpdateMapper.java)
* [FORTUNES](src/main/java/io/tadx/benchmark/route_mapper/FortunesRouteMapper1.java)



## Test URLs
### JSON

http://localhost:8000/json

### PLAINTEXT

http://localhost:8000/plaintext

### DB

http://localhost:8000/db

### QUERY

http://localhost:8000/query?queries=

### CACHED QUERY

http://localhost:8000/cached_query?queries=

### UPDATE

http://localhost:8000/update?queries=

### FORTUNES

http://localhost:8000/fortunes
