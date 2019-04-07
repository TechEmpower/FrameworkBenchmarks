# akka-http-slick-postgres Benchmarking Test

### Test Type Implementation Source Code

* [JSON](src/main/scala/net/benchmark/akka/http/world/JsonRoute.scala)
* [PLAINTEXT](src/main/scala/net/benchmark/akka/http/world/PlainTextRoute.scala)
* [DB](src/main/scala/net/benchmark/akka/http/world/DbRoute.scala)
* [QUERY](src/main/scala/net/benchmark/akka/http/world/QueriesRoute.scala)
* [UPDATE](src/main/scala/net/benchmark/akka/http/world/UpdateRoute.scala)
* [FORTUNES](src/main/scala/net/benchmark/akka/http/fortune/FortuneRoutes.scala)

## Important Libraries

The tests were run with:

* [Akka HTTP](https://doc.akka.io/docs/akka-http/current/)
* [Akka Streams](https://doc.akka.io/docs/akka/current/stream/)
* [Slick](http://slick.lightbend.com/)
* [HikariCP](https://brettwooldridge.github.io/HikariCP/)
* [Circe](https://circe.github.io/circe/)
* [Cats](https://github.com/typelevel/cats/)
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
