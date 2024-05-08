# Hummingbird 2 Benchmarking Test

Hummingbird 2 is a lightweight, flexible HTTP server framework written in Swift. HUmmingbird 2 is a complete rewrite using Swift concurrency.

### Test Type Implementation Source Code

* [JSON](src/Sources/server/main.swift)
* [PLAINTEXT](src/Sources/server/main.swift)
* [DB](src-postgres/Sources/server/Controllers/WorldController.swift)
* [Query](src-postgres/Sources/server/Controllers/WorldController.swift)
* [Updates](src-postgres/Sources/server/Controllers/WorldController.swift)
* [Fortunes](src-postgres/Sources/server/Controllers/FortunesController.swift)

## Important Libraries
This version of Hummingbird requires
* [Swift 5.10](https://swift.org)  
* [SwiftNIO 2.x](https://github.com/apple/swift-nio/)
In these tests for database access it uses
* [PostgresKit 2.21](https://github.com/vapor/postgres-nio/)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### Query

http://localhost:8080/queries?queries=

### Updates

http://localhost:8080/updates?queries=

### Fortunes

http://localhost:8080/fortunes
