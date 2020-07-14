# [Kitura](https://kitura.io) Benchmark Test

This is the [Kitura](https://kitura.io) portion of a [benchmarking test suite](https://www.techempower.com/benchmarks/) comparing a variety of web development platforms.

## Variants

The benchmark is split up into multiple executable targets, demonstrating different database backends and uses of the Kitura routing APIs:
- `kitura`: Implementations of Plaintext and JSON using 'raw' (Express-style) routing
- `kitura-postgres`: Implementation of database tests, using Postgres with Swift-Kuery (no ORM)
- `kitura-postgres-orm`: Equivalent implementation with Postgres and Swift-Kuery-ORM
- `kitura-postgres-orm-codable`: Equivalent implementation with Postgres, using [Codable Routing together with Swift-Kuery-ORM](https://developer.ibm.com/swift/2018/03/01/introducing-swift-kuery-orm/)
- `kitura-mongodb`: Implementation of database tests, using MongoDB with MongoKitten (no ORM)
- `kitura-nio`: `kitura` implementations of Plaintext and JSON run on [Kitura-NIO](https://github.com/IBM-Swift/Kitura-NIO)
- `kitura-nio-postgres`: Testing `kitura` implementations of database tests using Postgres on [Kitura-NIO](https://github.com/IBM-Swift/Kitura-NIO)

There are additional variants for each of the above implementations, with the '-gcd' suffix: These are compiled from the same source, but use the Grand Central Dispatch threading model (used by default on macOS) instead of a direct epoll implementation (the default on Linux).

Each listens on port 8080, and uses the common URLs described below.

## Versions and Dependencies

This version of the benchmark requires Swift 4.0.3 or higher, and uses the following versions of Kitura and dependencies:

- [Kitura 2.5](https://github.com/IBM-Swift/Kitura)
- [HeliumLogger 1.x](https://github.com/IBM-Swift/HeliumLogger)
- [Configuration 3.x](https://github.com/IBM-Swift/Configuration)
- [SwiftKueryPostgreSQL 1.x](https://github.com/IBM-Swift/Swift-Kuery-PostgreSQL)
- [SwiftKueryORM 0.x](https://github.com/IBM-Swift/Swift-Kuery-ORM)
- [KituraStencil 1.x](https://github.com/IBM-Swift/Kitura-StencilTemplateEngine)
- [KituraMustache 1.x](https://github.com/IBM-Swift/Kitura-MustacheTemplateEngine)
- [MongoKitten 4.x](https://github.com/OpenKitten/MongoKitten)

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
