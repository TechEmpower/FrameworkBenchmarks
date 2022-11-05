#http4k Benchmarking Test

## Infrastructure Software Versions
The tests were run with:

* JDK 11
* [http4k](https://http4k.org)

## Test URLs
- JSON Encoding: http://localhost:8080/json
- Single query: http://localhost:8080/db
- Multiple queries: http://localhost:8080/queries
- Fortunes: http://localhost:8080/fortunes
- Updates: http://localhost:8080/updates
- Cached: http://localhost:8080/cached
- Plaintext: http://localhost:8080/plaintext

## Supported backends (w/ Postgres client)
- SunHttp (default - bundled with core module - zero dependencies)
- Apache (5)
- Apache4
- KtorCIO
- KtorNetty
- Jetty
- Netty
- Ratpack
- Undertow

## How to run example
```bash
gradle clean build jetty:shadowJar
java -jar jetty/build/libs/http4k-jetty-benchmark.jar
```
