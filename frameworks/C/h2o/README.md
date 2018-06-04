# h2o

This is a framework implementation using the [H2O](https://h2o.examp1e.net) HTTP server. It builds directly on top of `libh2o` instead of running the standalone server.

## Requirements

[CMake](https://cmake.org), [H2O](https://h2o.examp1e.net), [libpq](https://www.postgresql.org), [mustache-c](https://github.com/x86-64/mustache-c), [OpenSSL](https://www.openssl.org), [YAJL](https://lloyd.github.io/yajl)

## Performance issues

### Database tests

`libpq` does not support command pipelining, and implementing anything equivalent on top of it conflicts with the requirements.

### Database updates

In the Citrine environment the database connection settings that improve the performance on the updates test make the other database results worse, and vice versa.

### Plaintext

`libh2o` performs at least one system call per pipelined response.

### Cached queries

Most of the operations that the in-memory caching mechanism provided by `libh2o` supports modify the cache (in particular, `h2o_cache_fetch()` updates a list of least recently used entries, and may remove expired ones), so when the application is running multithreaded, cache access must be serialized.

## Contact

Anton Kirilov <antonvkirilov@gmail.com>
