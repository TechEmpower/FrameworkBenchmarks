#fintrospect Benchmarking Test

## Infrastructure Software Versions
The tests were run with:

* [Java Oracle 1.8.0_25](http://www.oracle.com/technetwork/java/javase)
* [http4k](https://github.com/http4k/http4k)

## Test URLs

- JSON Encoding: http://localhost:9000/json
- Plaintext: http://localhost:9000/plaintext

## Supported backends
- Jetty
- Netty
- Undertow

## How to run
```bash
./gradlew clean build jetty
java -jar build/libs/http4k-standalone.jar &
```

