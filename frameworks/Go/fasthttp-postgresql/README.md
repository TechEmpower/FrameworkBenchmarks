# [fasthttp](https://github.com/valyala/fasthttp) (GoLang) Benchmarking Test for postgresql

This is the go portion of a [benchmarking test suite](https://www.techempower.com/benchmarks/) comparing a variety of web development platforms.

"Fasthttp is a fast http package for Go."

# This variant uses Postgres via Jack Christensen's pgx library

## Test URLs

    http://localhost:8080/json
    http://localhost:8080/db
    http://localhost:8080/queries?queries=[1-500]
    http://localhost:8080/fortunes
    http://localhost:8080/updates?queries=[1-500]
    http://localhost:8080/plaintext
