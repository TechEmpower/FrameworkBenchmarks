# Hummingbird Benchmarking Test

Hummingbird is a lightweight, flexible HTTP server framework written in Swift.

### Test Type Implementation Source Code

* [JSON](src/Sources/server/main.swift)
* [PLAINTEXT](src/Sources/server/main.swift)
* [DB](src-postgres/Sources/server/Controllers/WorldController.swift)
* [Query](src-postgres/Sources/server/Controllers/WorldController.swift)
* [Updates](src-postgres/Sources/server/Controllers/WorldController.swift)
* [Fortunes](src-postgres/Sources/server/Controllers/FortunesController.swift)

## Important Libraries
This version of Hummingbird requires
* [Swift 5.3](https://swift.org)  
* [SwiftNIO 2.x](https://github.com/apple/swift-nio/)
In these tests for database access it uses
* [PostgresKit 2.0](https://github.com/vapor/postgres-kit/)

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
