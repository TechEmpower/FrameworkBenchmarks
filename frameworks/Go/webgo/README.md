# web.go Benchmarking Test

This is the web.go portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

web.go is a simple way to write web applications in the Go programming language.

But if you're looking for the fastest router, this is probably not your best choice, since it uses the reflect-Package to call the handler functions and regular expressions for matching routes.

## JSON Encoding Test

- [JSON test source](src/hello/hello.go)

## Versions

- [web.go](https://github.com/JaCoB1123/web)

## Test URLs

### JSON Encoding Test

    http://localhost:8080/json
