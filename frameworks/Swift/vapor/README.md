# [Vapor](https://vapor.codes/) Benchmark Test

This is the Vapor portion of a [benchmarking tests suite](../../) comparing a variety of web development platforms.

### Targets

One executable target. Listens on port 8080.

### Database

PostgreSQL

## Versions

[Swift 5.7](https://swift.org/)
[Vapor 4](https://vapor.codes/)

## Test URLs: `vapor`

### Plaintext test

http://localhost:8080/plaintext

#### JSON serialization test

http://localhost:8080/json

### Single database query test

http://localhost:8080/db

### Multiple database queries test

http://localhost:8080/queries?queries=[1...500]

### Database updates test

http://localhost:8080/updates?queries=[1...500]
