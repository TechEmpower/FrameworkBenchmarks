# [Indigo](https://github.com/indigo-web/indigo) (Go) Benchmarking Test

This is the go portion of a [benchmarking test suite](https://www.techempower.com/benchmarks/) comparing a variety of web development platforms.

> Indigo is a web-framework focusing at performance, elegancy and robustness.

## Test URLs
* http://localhost:8080/json
* http://localhost:8080/db
* http://localhost:8080/query?n=[1-500]
* http://localhost:8080/update?n=[1-500]
* http://localhost:8080/cached-query?n=[1-500]
* http://localhost:8080/fortune
* http://localhost:8080/plaintext
