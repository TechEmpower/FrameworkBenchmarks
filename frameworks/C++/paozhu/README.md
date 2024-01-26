# paozhu Benchmarking Test

This is the [Paozhu](https://github.com/hggq/paozhu) 

This Benchmarking Test code from https://github.com/hggq/paozhu/releases/tag/v1.5.8

### Test Type Implementation Source Code

* [Benchmark code](controller/src/techempower.cpp)
* [ORM config](conf/orm.conf) 
* [Server config](conf/server.conf) 

## Test URLs
### JSON

http://localhost:8888/json

### PLAINTEXT

http://localhost:8888/plaintext


### Single Database Query

http://localhost:8888/db

### Fortune

http://localhost:8888/fortunes

### Multiple Database Queries

http://localhost:8888/queries?queries=10

### Database Updates

http://localhost:8888/updates?queries=10

### Cache
http://localhost:8888/cached-queries?count=20