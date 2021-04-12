# [go-std](GoLang) Benchmarking Test

This is the go portion of a [benchmarking test suite](https://www.techempower.com/benchmarks/) comparing a variety of web development platforms.

"Golang standard library. For database tests PostgreSQL, MySQL and MongoDB were used."

## Test URLs

    http://localhost:8080/json
    http://localhost:8080/db
    http://localhost:8080/queries?queries=[1-500]
    http://localhost:8080/update?queries=[1-500]
    http://localhost:8080/plaintext
    http://localhost:8080/fortunes
