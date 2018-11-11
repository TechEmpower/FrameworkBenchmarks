# facil.io Benchmarking Test

Benchmarks for the [facil.io framework](http://facil.io).

### Test Type Implementation Source Code

* [JSON](bench_app.c)
* [PLAINTEXT](bench_app.c)

*Missing* tests:

* [DB](bench_app.c)
* [QUERY](bench_app.c)
* [CACHED QUERY](bench_app.c)
* [UPDATE](bench_app.c)
* [FORTUNES](bench_app.c)

## Important Libraries
The tests were run with:
* [facil.io](http://facil.io)

## Adding Tests

When adding tests, please remember that:

* [facil.io](http://facil.io) was designed to be evented.

    For example, instead of waiting for Database results, please use `http_pause` and `http_resume` to pause a request from processing until the Database results arrived (at which point, remember to resume the request).

    If using the evented approach is impossible, add threads by updating the command line `-t` argument to an increased thread count.

* [facil.io](http://facil.io) assumes memory ownership oriented design.

    This means that functions that place data into another object (i.e., setting header values, hash values etc') take ownership of the data *but not it's key*.

    For example, when adding an HTTP header, the header **name** (key) is owned by the calling function (which should free the header name when it's done with it) - but the header **value** is now owned by the header collection and can no longer be used by the calling function (unless duplicated using `fiobj_dup`).

    This only applies to functions that place the data into an object (see `FIOBJ` documentation). This doesn't apply to send/receive functions such as `pubsub_publish` or pub/sub handlers where the data is "passed through" (or copied) without being placed into an accessible object.

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

<!--
### DB

(Not implemented for this version)

http://localhost:8080/db

### QUERY

(Not implemented for this version)

http://localhost:8080/query?queries=

### CACHED QUERY

(Not implemented for this version)

http://localhost:8080/cached_query?queries=

### UPDATE

(Not implemented for this version)

http://localhost:8080/update?queries=

### FORTUNES

(Not implemented for this version)

http://localhost:8080/fortunes -->
