# [Echo](https://github.com/labstack/echo) (Go) Benchmarking Test

This is the go portion of a [benchmarking test suite](https://www.techempower.com/benchmarks/) comparing a variety of web development platforms.

> High performance, extensible, minimalist Go web framework

## Test URLs

- http://localhost:8080/json
- http://localhost:8080/db
- http://localhost:8080/queries/:n[1-500]
- http://localhost:8080/fortunes
- http://localhost:8080/updates/:n[1-500]
- http://localhost:8080/plaintext
