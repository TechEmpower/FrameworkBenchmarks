# [iris](https://github.com/kataras/iris) (GoLang) Benchmarking Test

This is the go portion of a [benchmarking test suite](https://www.techempower.com/benchmarks/) comparing a variety of web development platforms.

"Iris is a web framework for Go using Postgres."

## Test URLs

    http://localhost:8080/json
    http://localhost:8080/db
    http://localhost:8080/queries?queries=[1-500]
    http://localhost:8080/updates?queries=[1-500]
    http://localhost:8080/plaintext