# [Kitura](https://kitura.io) Benchmark Test

This is the [Kitura](https://kitura.io) portion of a [benchmarking test suite](https://www.techempower.com/benchmarks/) comparing a variety of web development platforms.

## Variants

There are two versions of the benchmark, using different database backends:
- `kitura`: Kitura with Postgres
- `kitura-mongokitten`: Kitura with MongoDB

There are two additional variants: `kitura-gcd` and `kitura-gcd-mongokitten`. These are compiled from the same source, but use the Grand Central Dispatch threading model (used by default on macOS) instead of the default epoll implementation on Linux.

Each listens on port 8080, and use common URLs described below.

## Versions and Dependencies

This version of the benchmark requires Swift 4.0.3 or Swift 4.1, and uses the following versions of Kitura and dependencies:

- [Kitura 2.3](https://github.com/IBM-Swift/Kitura)
- [HeliumLogger 1.7](https://github.com/IBM-Swift/HeliumLogger)
- [Configuration 3.0](https://github.com/IBM-Swift/Configuration)
- [SwiftKueryPostgreSQL 1.1](https://github.com/IBM-Swift/Swift-Kuery-PostgreSQL)
- [KituraStencil 1.9](https://github.com/IBM-Swift/Kitura-StencilTemplateEngine)
- [KituraMustache 1.7](https://github.com/IBM-Swift/Kitura-MustacheTemplateEngine)
- [MongoKitten 4.1.3](https://github.com/OpenKitten/MongoKitten)

## Test URLs
### JSON serialization

http://localhost:8080/json

### Single database query

http://localhost:8080/db

### Multiple database queries

http://localhost:8080/queries?queries=n

### Fortunes

http://localhost:8080/fortunes

### Database updates

http://localhost:8080/updates?queries=n

### Plaintext

http://localhost:8080/plaintext
