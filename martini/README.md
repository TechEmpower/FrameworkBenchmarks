# Go Benchmarking Test using Martini

This is the Go (using [go-martini](https://github.com/go-martini/martini)) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Main files
* [Main application - app.go](src/app/app.go)
* [Models - models.go](src/app/models.go)

## Versions

* [Go 1.2.1](http://golang.org/)
* [go-martini](https://github.com/go-martini/martini)
* [martini-contrib/render](https://github.com/martini-contrib/render)

## Test URLs

### JSON Encoding Test

http://localhost:8080/json

### Simple Query Test

http://localhost:8080/db

### Multiple Query Test

http://localhost:8080/queries
http://localhost:8080/queries?queries=100

`queries` from `1` to `500`

### Fortunes Test

http://localhost:8080/fortunes

### Update Test

http://localhost:8080/updates
http://localhost:8080/updates?queries=100

`queries` from `1` to `500`

### Plain Text Test

http://localhost:8080/plaintext

