# [Perfect](https://www.perfect.org) Benchmark Test

This is the [Perfect](https://www.perfect.org) portion of a [benchmarking test suite](https://www.techempower.com/benchmarks/) comparing a variety of web development platforms.

## Variants

There is are four versions of the benchmark, three of which use databases:
- `Perfect`: No DB
- `Perfect-MySQL`: Using MySQL
- `Perfect-PostgreSQL`: Using PostgreSQL
- `Perfect-MongoDB`: Using MongoDB

Each listens on port 8080, and use common URLs described below.

## Versions and Dependencies

This version of the benchmark requires Swift 4.1, and uses the following versions of Perfect and dependencies:

- [Perfect 3.0](https://github.com/PerfectlySoft/Perfect-HTTPServer.git)
- [Perfect-MySQL 3.0](https://github.com/PerfectlySoft/Perfect-MySQL.git)
- [Perfect-PostgreSQL 3.0](https://github.com/PerfectlySoft/Perfect-PostgreSQL.git)
- [Perfect-MongoDB 3.0](https://github.com/PerfectlySoft/Perfect-MongoDB.git)

## Test URLs
### JSON serialization

http://localhost:8080/json

### Plaintext

http://localhost:8080/plaintext

### DB Store - on all configurations

http://localhost:8080/db

### Queries - on all configurations

http://localhost:8080/queries/queries=2

### Updates - on all configurations

http://localhost:8080/updates/queries=2