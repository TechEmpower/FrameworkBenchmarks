# Granian Benchmark Test

This is the Granian portion of a [benchmarking tests suite](../../) comparing a variety of web development platforms.

The information below is specific to Granian. For further guidance, review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).

Also note that there is additional information provided in the [Python README](../).

## Description

[Granian](https://github.com/emmett-framework/granian) is an asyncio server for Python applications.

## Test Paths & Source

Granian includes two different implementations:

- ASGI implementation in the [app\_asgi.py](app_asgi.py)
- RSGI implementation in the [app\_rsgi.py](app_rsgi.py)

Both implementations includes the following tests:

* JSON Serialization: "/json"
* Plaintext: "/plaintext"

*Replace # with an actual number.*

## Resources

* [Github repository](https://github.com/emmett-framework/granian)
