# [kame](https://github.com/guregu/kami) (Go) Benchmarking Test

This is the go portion of a [benchmarking test suite](../) comparing a variety of web development platforms.


"kami (神) is a tiny web framework using [x/net/context](https://blog.golang.org/context) for request context and [HttpRouter](https://github.com/julienschmidt/httprouter) for routing. It includes a simple system for running hierarchical middleware before and after requests, in addition to log and panic hooks."


### Source
* [All test source](src/kami/server.go)

## Test URLs

    http://localhost:8080/json
    http://localhost:8080/db
    http://localhost:8080/queries?queries=[1-500]
    http://localhost:8080/fortunes
    http://localhost:8080/updates?queries=[1-500]
    http://localhost:8080/plaintext
