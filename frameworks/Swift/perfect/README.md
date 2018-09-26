# [Perfect](https://www.perfect.org) Benchmark Test

This is the [Perfect](https://www.perfect.org) portion of a [benchmarking test suite](https://www.techempower.com/benchmarks/) comparing a variety of web development platforms.

## Variants

There is one version of the benchmark, not using any backend:
- `Perfect`: No DB

Each listens on port 8080, and use common URLs described below.

## Versions and Dependencies

This version of the benchmark requires Swift 4.1, and uses the following versions of Perfect and dependencies:

- [Perfect 3.0](https://github.com/PerfectlySoft/Perfect-HTTPServer.git)

## Test URLs
### JSON serialization

http://localhost:8080/json

### Plaintext

http://localhost:8080/plaintext
