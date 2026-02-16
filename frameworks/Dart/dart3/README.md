# Dart 3 Benchmarking Test

## Important Libraries

The tests were run with:

- [Dart v3.11.0](https://dart.dev/)

## Benchmark Variants

### Native

Minimal implementation with the smallest resource footprint.
Supports basic horizontal scaling via [Isolates](https://dart.dev/language/isolates) and socket sharing.
([source code](https://github.com/TechEmpower/FrameworkBenchmarks/tree/master/frameworks/Dart/dart3/dart_native))

Test URLs:

- JSON: `http://localhost:8080/json`
- PLAINTEXT: `http://localhost:8080/plaintext`

### AOT

Performance-oriented AOT implementation for superior horizontal scaling.
Achieves lowest latency and higher throughput with a slightly larger footprint.
([source code](https://github.com/TechEmpower/FrameworkBenchmarks/tree/master/frameworks/Dart/dart3/dart_aot))

Test URLs:

- JSON: `http://localhost:8080/json`
- PLAINTEXT: `http://localhost:8080/plaintext`

### Hybrid

Uses Dart Native, but leverages a load balancer to bypass horizontal scaling limits.
Designed for better hardware utilization despite increased complexity and overhead.
([source code](https://github.com/TechEmpower/FrameworkBenchmarks/tree/master/frameworks/Dart/dart3/dart_hybrid))

Test URLs:

- JSON: `http://localhost:8080/json`
- PLAINTEXT: `http://localhost:8080/plaintext`
