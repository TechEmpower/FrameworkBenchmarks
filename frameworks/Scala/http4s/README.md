# http4s Benchmarking Tests

- [Main Entrypoint](src/main/scala/http4s/techempower/benchmark/Server.scala)
- [JSON Encoding Test]((src/main/scala/http4s/techempower/benchmark/http/JsonHttpEndpoint.scala))
- [Fortune Test]((src/main/scala/http4s/techempower/benchmark/http/FortuneHttpEndpoint.scala))
- [Plaintext Test]((src/main/scala/http4s/techempower/benchmark/http/PlaintextHttpEndpoint.scala))
- [Database Select and Updates Test]((src/main/scala/http4s/techempower/benchmark/http/DatabaseHttpEndpoint.scala))
- [Queries Test]((src/main/scala/http4s/techempower/benchmark/http/QueriesHttpEndpoint.scala))




## Infrastructure Software Versions
The tests were run with:

- [http4s 0.18.2](http://http4s.org/)
- [blaze 0.12.11](https://github.com/http4s/blaze/)
- [cats 1.0.1](https://typelevel.org/cats)
- [doobie 0.5.1](https://tpolecat.github.io/doobie/)
- [circe 0.9.2](https://circe.github.io/circe/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/json

### Plaintext Test

http://localhost:8080/plaintext

## How to run
sbt assembly

sbt run

\*- or -*

sbt assembly

java -jar target/scala-2.12/http4s-benchmark.jar "$DBHOST"

## Maintenance

The last update was done by 03/08/2018. Please update these tests regularly, as they affect
the public perception of the quality of these products. Updating the middleware and choice
of optimization strategy as new information becomes available is up to the test writer, but
please consult with the previous testers for information regarding previous strategies.

We can be found in various chatrooms and social media - any of the following will do:

- [Scalaz Irc](http://irc.freenode.net/scalaz)
- [Scalaz Gitter](https://gitter.im/scalaz/scalaz)
- [Http4s Gitter](https://gitter.im/http4s/http4s)

For direct contact, see [emilypi](emilypi@cohomolo.gy)