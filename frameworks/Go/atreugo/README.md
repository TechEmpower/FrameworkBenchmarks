# [atreugo](https://github.com/savsgio/atreugo) (GoLang) Benchmarking Test

This is the go portion of a [benchmarking test suite](https://www.techempower.com/benchmarks/) comparing a variety of web development platforms.

"High performance and extensible micro web framework. Zero memory allocations in hot paths."

## Test URLs

    http://localhost:8080/json
    http://localhost:8080/db
    http://localhost:8080/queries?queries=[1-500]
    http://localhost:8080/cached-worlds?queries=[1-500]
    http://localhost:8080/fortunes
    http://localhost:8080/updates?queries=[1-500]
    http://localhost:8080/plaintext
