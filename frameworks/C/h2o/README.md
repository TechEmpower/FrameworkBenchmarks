# h2o

This is a framework implementation using the [H2O](https://h2o.examp1e.net) HTTP server. It builds directly on top of `libh2o` instead of running the standalone server.

## Requirements

[CMake](https://cmake.org), [H2O](https://h2o.examp1e.net), [libpq](https://www.postgresql.org), [mustache-c](https://github.com/x86-64/mustache-c), [OpenSSL](https://www.openssl.org), [YAJL](https://lloyd.github.io/yajl)

## Test implementations

The test implementations are located into the `src/handlers` directory.

## Performance tuning

If the test environment changes, it will probably be necessary to tune some of the framework settings in order to achieve the best performance possible. The most significant parameter is the maximum number of database connections per thread, which is controlled by the `DB_CONN` variable in the `h2o.sh` script.

## Performance issues

### Database tests

`libpq` does not support command pipelining, and implementing anything equivalent on top of it conflicts with the requirements.

### Database updates

In the Citrine environment the database connection settings that improve the performance on the updates test make the other database results worse, and vice versa.

### Plaintext

`libh2o` performs at least one system call per pipelined response.

## Contact

Anton Kirilov <antonvkirilov@gmail.com>
