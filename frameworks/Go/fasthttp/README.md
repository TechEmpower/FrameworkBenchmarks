# [fasthttp](https://github.com/valyala/fasthttp) (GoLang) Benchmarking Test

This is the go portion of a [benchmarking test suite](https://www.techempower.com/benchmarks/) comparing a variety of web development platforms.

"Fast HTTP package for Go. Tuned for high performance. Zero memory allocations in hot paths. Up to 10x faster than net/http"

## Test URLs

    http://localhost:8080/json
    http://localhost:8080/db
    http://localhost:8080/queries?queries=[1-500]
    http://localhost:8080/cached-worlds?queries=[1-500]
    http://localhost:8080/fortunes
    http://localhost:8080/updates?queries=[1-500]
    http://localhost:8080/plaintext
