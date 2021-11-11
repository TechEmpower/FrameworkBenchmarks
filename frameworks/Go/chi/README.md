# [Chi](https://github.com/go-chi/chi) (Go) Benchmarking Test

This is the go portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

"Chi is a lightweight, idiomatic and composable router for building Go HTTP services."

### Source
* [All test source](src/chi/server.go)

## Test URLs

    http://localhost:8080/json
    http://localhost:8080/db
    http://localhost:8080/queries?queries=[1-500]
    http://localhost:8080/fortunes
    http://localhost:8080/updates?queries=[1-500]
    http://localhost:8080/plaintext
