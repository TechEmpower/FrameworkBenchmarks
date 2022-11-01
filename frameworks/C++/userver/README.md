# userver Benchmarking Test

This is the [userver](https://github.com/userver-framework/userver) portion of a [benchmarking test suite](https://github.com/TechEmpower/FrameworkBenchmarks) comparing a variety of web development platforms.

### Test Type Implementation Source Code

* [PLAINTEXT](userver_benchmark/src/controllers/plaintext/handler.cpp)
* [JSON](userver_benchmark/src/controllers/json/handler.cpp)
* [Single Database Query](userver_benchmark/src/controllers/single_query/handler.cpp)
* [Multiple Database Queries](userver_benchmark/src/controllers/multiple_queries/handler.cpp)
* [Database Updates](userver_benchmark/src/controllers/updates/handler.cpp)
* [Cached Queries](userver_benchmark/src/controllers/cached_queries/handler.cpp)

## Test URLs
### PLAINTEXT

http://localhost:8080/plaintext

### JSON

http://localhost:8080/json

### Single Database Query

http://localhost:8080/db

### Multiple Database Queries

http://localhost:8080/queries

### Database Updates

http://localhost:8080/updates

### Cached Queries

http://localhost:8080/cached-queries

