# [Pine](https://github.com/xiusin/pine) (Golang) Benchmarking Test

This is the go portion of a [benchmarking test suite](https://www.techempower.com/benchmarks/) comparing a variety of web development platforms.

`Pine` is a modular, full-featured and production-ready application development framework of golang(base on `net/http`). Providing a series of core components and dozens of 
practical modules, such as: mvc, cache, logger, database orm, render engine, di. Supporting web server integrated with router, cookie, 
session, middleware, logger, configure, template, rewrites and many more features."

## Test URLs
    http://localhost:8080/json
    http://localhost:8080/db
    http://localhost:8080/queries?queries=[1-500]
    http://localhost:8080/fortunes
    http://localhost:8080/updates?queries=[1-500]
    http://localhost:8080/plaintext
