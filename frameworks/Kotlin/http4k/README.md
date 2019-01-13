#fintrospect Benchmarking Test

## Infrastructure Software Versions
The tests were run with:

* JDK 11
* [http4k](https://http4k.org)

## Test URLs

- JSON Encoding: http://localhost:9000/json
- Single query: http://localhost:9000/db
- Multiple queries: http://localhost:9000/queries
- Fortunes: http://localhost:9000/fortunes
- Updates: http://localhost:9000/updates
- Plaintext: http://localhost:9000/plaintext


## Supported backends
- Apache (w/ Postgres + Reactive PG clienta)
- KtorCIO (w/ Postgres client)
- Jetty (w/ Postgres client)
- Netty (w/ Postgres client)
- Undertow (w/ Postgres client)

## How to run example
```bash
gradle clean build jetty:shadowJar
java -jar jetty/build/libs/http4k-jetty-benchmark.jar
```
