# Tapir Benchmarking Test

This is a simple test to benchmark the performance of the Tapir libraries along with different backends in Scala.

### Test Type Implementation Source Code

* JSON
* PLAINTEXT

## Software Versions

* [Java OpenJDK 21](https://adoptium.net/temurin/releases/)
* [Scala 3.6.4](https://www.scala-lang.org/)
* [Tapir 1.11.24](https://tapir.softwaremill.com)

### Backend Implementations
* [Tapir running as zio-http server](https://tapir.softwaremill.com/en/latest/server/ziohttp.html)
* [Tapir running as http4s server](https://tapir.softwaremill.com/en/latest/server/http4s.html)
* [as an http4s server using ZIO](https://tapir.softwaremill.com/en/latest/server/zio-http4s.html)
* [Tapir running as Netty-based server(Cats)](https://tapir.softwaremill.com/en/latest/server/netty.html)
* [Tapir running as Netty-based server(ZIO)](https://tapir.softwaremill.com/en/latest/server/netty.html)
* [Tapir running as pekko-http server](https://tapir.softwaremill.com/en/latest/server/pekkohttp.html)

## Test URLs

* JSON - http://localhost:8080/json
* PLAINTEXT - http://localhost:8080/plaintext
