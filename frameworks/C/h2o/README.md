# h2o

This is a framework implementation using the [H2O](https://h2o.examp1e.net/) HTTP server.

## Requirements

CMake, H2O, libpq, mustache-c, OpenSSL, YAJL

## Performance issues

### Plaintext

H2O performs at least one system call per pipelined response.

### Cached queries

The in-memory caching mechanism provided by libh2o uses mutexes even for read-only access, so when the application is running multithreaded, cache usage is serialized.

## Contact

Anton Kirilov <antonvkirilov@gmail.com>
