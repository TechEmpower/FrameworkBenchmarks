# Go Falcore Benchmarking Test

This is the go Falcore portion of a [benchmarking test suite](../) comparing a variety of web development platforms.
Falcore is an HTTP server written in Go that provides an alternate API and additional features compared to the Go standard
library HTTP server.

### Source
* [All test source](src/framework_benchmarks/falcore.go)

## Versions

* [Go 1.1.1](http://golang.org/)

## Test URLs

    http://localhost:8080/json
    http://localhost:8080/db
    http://localhost:8080/fortune
    http://localhost:8080/queries
    http://localhost:8080/update
    http://localhost:8080/plaintext

