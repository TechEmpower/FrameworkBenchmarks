# Uvicorn Benchmark Test

This is the Uvicorn portion of a [benchmarking tests suite](../../)
comparing a variety of web development platforms.

The information below is specific to Uvicorn. For further guidance,
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note that there is additional information provided in
the [Python README](../).

## Description

[Uvicorn](https://github.com/tomchristie/uvicorn) is a lightning fast
asyncio server for Python 3.

## Implementation

Uvicorn is implemented using:

* Gunicorn for process managment.
* The uvloop event loop.
* The httptools HTTP parsing library.
* The ASGI consumer interface, for interacting with the application layer.

## Test Paths & Sources

All of the test implementations are located within a single file ([app.py](app.py)).

* [JSON Serialization](app.py): "/json"
* [Plaintext](app.py): "/plaintext"

## Resources

* [Repo](https://github.com/tomchristie/uvicorn)
