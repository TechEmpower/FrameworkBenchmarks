# userver Benchmarking Test

This is the [userver](https://github.com/userver-framework/userver) portion of a [benchmarking test suite](https://github.com/TechEmpower/FrameworkBenchmarks) comparing a variety of web development platforms.

This benchmarks comes in two configurations: **userver** and **userver-bare**, where both configurations use exactly the same handlers code, but **userver-bare** replaces default http implementation of **userver** with custom one.  
You see, **userver** being feature-rich framework widely used in production comes with a lot of useful functionality built-in (metrics, dynamic configuring, logging/tracing, congestion control etc...) none of which is of any use in benchmarks; although most of that can be disabled via configs, some parts remain, and these parts aren't free.  
The aim of **userver-bare** is to explore practical limits of lower-level **userver** functionality when performance is an absolute must, while still being idiomatic userver code.

### Test Type Implementation Source Code

* [Plaintext](userver_benchmark/controllers/plaintext/handler.cpp)
* [Json](userver_benchmark/controllers/json/handler.cpp)
* [Single Database Query](userver_benchmark/controllers/single_query/handler.cpp)
* [Multiple Database Queries](userver_benchmark/controllers/multiple_queries/handler.cpp)
* [Database Updates](userver_benchmark/controllers/updates/handler.cpp)
* [Cached Queries](userver_benchmark/controllers/cached_queries/handler.cpp)

## Test URLs
### Plaintext

http://localhost:8080/plaintext

### Json

http://localhost:8080/json

### Single Database Query

http://localhost:8080/db

### Multiple Database Queries

http://localhost:8080/queries

### Database Updates

http://localhost:8080/updates

### Cached Queries

http://localhost:8080/cached-queries

