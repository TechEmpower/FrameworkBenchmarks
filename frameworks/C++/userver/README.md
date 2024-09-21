# userver Benchmarking Test

This is the [userver](https://github.com/userver-framework/userver) portion of a [benchmarking test suite](https://github.com/TechEmpower/FrameworkBenchmarks) comparing a variety of web development platforms.

### Test Type Implementation Source Code

* [Plaintext](userver_benchmark/controllers/plaintext/handler.cpp)
* [Json](userver_benchmark/controllers/json/handler.cpp)
* [Fortunes](userver_benchmark/controllers/fortunes/handler.cpp)
* [Single Database Query](userver_benchmark/controllers/single_query/handler.cpp)
* [Multiple Database Queries](userver_benchmark/controllers/multiple_queries/handler.cpp)
* [Database Updates](userver_benchmark/controllers/updates/handler.cpp)
* [Cached Queries](userver_benchmark/controllers/cached_queries/handler.cpp)

## Test URLs
### Plaintext

http://localhost:8080/plaintext

### Json

http://localhost:8080/json

### Fortunes

http://localhost:8080/fortunes

### Single Database Query

http://localhost:8080/db

### Multiple Database Queries

http://localhost:8080/queries

### Database Updates

http://localhost:8080/updates

### Cached Queries

http://localhost:8080/cached-queries

