# Curacao Benchmarking Test

Curacao is an open-source toolkit for building REST/HTTP-based integration layers on top of asynchronous Servlets.

## Versions

Curacao 2.6.3
https://github.com/markkolich/curacao

## Tests

### JSON Serialization

Uses [Google's GSON](https://code.google.com/p/google-gson/) under-the-hood.

See the `json` method in [Benchmarks.java](src/main/java/benchmark/Benchmarks.java)

    http://localhost:8080/json

### Plaintext

See the `plainText` method in [Benchmarks.java](src/main/java/benchmark/Benchmarks.java)

    http://localhost:8080/plaintext

