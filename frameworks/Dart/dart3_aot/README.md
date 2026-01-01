# Dart 3 AOT Benchmarking Test

## Test Type Implementation Source Code

- [JSON](server.dart)
- [PLAINTEXT](server.dart)

## Important Libraries

The tests were run with:

- [Dart v3.10.3](https://dart.dev/)

## Test URLs

### JSON

`http://localhost:8080/json`

### PLAINTEXT

`http://localhost:8080/plaintext`

## Why testing AOT?

In the current version Dart, when compiled to a self-contained executable, cannot fully take advantage of all the available threads available in TechEmpower Web Framework Benchmarks' [environment](https://www.techempower.com/benchmarks/#section=environment), see this [issue](https://github.com/dart-lang/sdk/issues/60815) for more details.
