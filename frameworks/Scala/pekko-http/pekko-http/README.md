# pekko-http Benchmarking Test

### Test Type Implementation Source Code

* [JSON](src/main/scala/pekko/http/benchmark/handlers/JsonHandler.scala)
* [PLAINTEXT](src/main/scala/pekko/http/benchmark/Bootstrap.scala)
* [DB](src/main/scala/pekko/http/benchmark/handlers/DbHandler.scala)
* [QUERY](src/main/scala/pekko/http/benchmark/handlers/QueriesHandler.scala)
* [UPDATE](src/main/scala/pekko/http/benchmark/handlers/UpdatesHandler.scala)
* [FORTUNES](src/main/scala/pekko/http/benchmark/handlers/FortunesHandler.scala)

## Important Libraries

The tests were run with:

* [Pekko HTTP](https://pekko.apache.org/docs/pekko-http/current/)
* [Pekko Streams](https://pekko.apache.org/docs/pekko/current/)
* [HikariCP](https://brettwooldridge.github.io/HikariCP/)
* [jsoniter-scala](https://github.com/plokhotnyuk/jsoniter-scala)
* [Scalate](https://scalate.github.io/scalate/)

## Test URLs

### JSON

http://localhost:9000/json

### PLAINTEXT

http://localhost:9000/plaintext

### DB

http://localhost:9000/db

### QUERY

http://localhost:9000/query?queries=

### CACHED QUERY

http://localhost:9000/cached_query?queries=

### UPDATE

http://localhost:9000/update?queries=

### FORTUNES

http://localhost:9000/fortunes
