# Vibe.D Benchmarking Test

This is the Vibe.D portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test controller/view](source/app.d)

### Data-Store/Database Mapping Test

* [DB test controller/model](source/app.d)

## Infrastructure Software Versions
The tests were run with:
* [Vibe.D v0.7.19](http://vibed.org/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/json

### Plaintext Test

http://localhost:8080/plaintext

### Data-Store/Database Mapping Test

MongoRaw:
http://localhost:8080/db

### Variable Query Test

MongoDB Raw:
http://localhost:8080/queries?queries=5
