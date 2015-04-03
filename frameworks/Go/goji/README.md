# [Goji](https://github.com/zenazn/goji) (Go) Benchmarking Test

This is the go portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

"Goji is a minimalistic web framework that values composability and simplicity."

### Source
* [All test source](src/goji/server.go)

## Test URLs

    http://localhost:8080/json
    http://localhost:8080/db
    http://localhost:8080/queries?queries=[1-500]
    http://localhost:8080/fortunes
    http://localhost:8080/updates?queries=[1-500]
    http://localhost:8080/plaintext