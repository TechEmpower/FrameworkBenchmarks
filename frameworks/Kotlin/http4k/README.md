#fintrospect Benchmarking Test

## Infrastructure Software Versions
The tests were run with:

* JDK 11
* [http4k](https://http4k.org)

## Test URLs

- JSON Encoding: http://localhost:9000/json
- Plaintext: http://localhost:9000/plaintext

## Supported backends
- Apache
- KtorCIO
- Jetty
- Netty
- SunHttp
- Undertow

## How to run example
```bash
gradle clean build jetty:shadowJar
java -jar jetty/build/libs/http4k-jetty-benchmark.jar
```
