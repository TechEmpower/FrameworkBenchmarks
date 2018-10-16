# NestJS Benchmarking Test

This is the Nest portion of a [benchmarking test suite](../).


## Infrastructure Software Versions
The tests were run with:
* [NestJS v5.1.0](https://docs.nestjs.com/)
* [pg 7.5.0](https://node-postgres.com/)

## Resources
* http://nodejs.org/api/cluster.html

## Test URLs
### JSON Encoding Test

http://localhost:8080/bench/json

### Data-Store/Database Mapping Test

PostgreSQL:
http://localhost:8080/bencg/db

### Variable Query Test

PostgreSQL:
http://localhost:8080/bench/queries?queries=2

### Update Query Test

PostgreSQL:
http://localhost:8080/bench/updates?queries=2