# [Vapor](https://vapor.codes/) Benchmark Test

This is the Vapor portion of a [benchmarking tests suite](../../) comparing a variety of web development platforms.

### Targets

Two executable targets. Each listens on port 8080.
`vapor-tfb` is for a bare server, (neither Fluent nor Leaf are dependencies)
`vapor-tfb-mysql` is for MySQL

### Dependencies

Linked MySQL client libraries are required to build the app, please consult Vapor's documentation [for MySQL](https://github.com/vapor/mysql).

### Database

MySQL

## Versions
[Swift 4.1.0](https://swift.org/)
[Vapor 3.0.0 gm](https://vapor.codes/)

## Test URLs: `vapor-tfb`

### Plaintext test
http://localhost:8080/plaintext

#### JSON serialization test
http://localhost:8080/json

## Test URLs: `vapor-tfb-mysql`

### Single database query test
http://localhost:8080/db

### Multiple database queries test
http://localhost:8080/queries/[1...500]

### Fortunes test
http://localhost:8080/fortunes

### Database updates test
http://localhost:8080/updates/[1...500]
