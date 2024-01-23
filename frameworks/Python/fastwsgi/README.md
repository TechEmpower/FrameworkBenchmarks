# FastWSGI Benchmark Test

This is the FastWSGI portion of a [benchmarking tests suite](../../)
comparing a variety of web development platforms.

The information below is specific to FastWSGI. For further guidance,
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note that there is additional information provided in
the [Python README](../).

## Description

[FastWSGI](https://github.com/jamesroberts/fastwsgi) is a lightning fast
asyncio server for Python 3 based on [libuv](https://github.com/libuv/libuv) library.

## Implementation

FastWSGI is implemented using:

* The [libuv](https://github.com/libuv/libuv) - multi-platform C library that provides support for asynchronous I/O based on event loops.
* The [llhttp](https://github.com/nodejs/llhttp) - fastest HTTP parsing library.
* The WSGI consumer interface, for interacting with the application layer.

## Test Paths & Sources

All of the test implementations are located within a single file ([app.py](app.py)).

* [JSON Serialization](app.py): "/json"
* [Plaintext](app.py): "/plaintext"

## Resources

* [FastWSGI on Github](https://github.com/jamesroberts/fastwsgi)
* [Benchmarks](https://github.com/jamesroberts/fastwsgi/blob/main/performance_benchmarks/PERFORMANCE.md)
