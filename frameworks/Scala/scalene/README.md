# Scalene Benchmarking Test

Scalene is a lightweight reactive HTTP server framework written in Scala.  It is
implemented directly on Java NIO and handles all TCP connection management,
message parsing/encoding, and request routing.

https://github.com/DanSimon/scalene

## Test Source Code Organization

All source for this test is in [Benchmark.scala](src/main/scala/Benchmark.scala)

Dependencies are defined in [build.sbt](build.sbt).

## Test URLs

* Plaintext - `http://localhost:8080/plaintext`
* JSON - `http://localhost:8080/json`
* DB - `http://localhost:8080/db`
* Queries - `http://localhost:8080/queries/<queries>`
