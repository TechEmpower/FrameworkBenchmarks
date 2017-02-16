# [Vapor](https://vapor.codes/) Benchmark Test

This is the Vapor portion of a [benchmarking tests suite](../../) comparing a variety of web development platforms.

### Targets

Three executable targets. Each listens on port 8080. All handle same URLs.  
`vapor-tfb-mysql` is for MySQL  
`vapor-tfb-postgresql` is for PostgreSQL  
`vapor-tfb-mongodb` is for MongoDB

### Dependencies

Linked MySQL and PostgreSQL client libraries are required to build the app, please consult Vapor's documentation [for MySQL](https://github.com/vapor/mysql), [for PostgreSQL](https://github.com/vapor/postgresql).

### Database

MySQL  
PostgreSQL  
MongoDB

## Versions
[Swift 3.0.2](http://swift.org/)
[Vapor 1.5](https://vapor.codes/)

## Test URLs

### JSON serialization test
http://localhost:8080/json

### Single database query test
http://localhost:8080/db

### Multiple database queries test
http://localhost:8080/queries?queries=[1...500]

### Fortunes test
http://localhost:8080/fortunes

### Database updates test
http://localhost:8080/updates?queries=[1...500]

### Plaintext test
http://localhost:8080/plaintext
