# Granian Benchmark Test

This is the Granian portion of a [benchmarking tests suite](../../) comparing a variety of web development platforms.

The information below is specific to Granian. For further guidance, review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).

Also note that there is additional information provided in the [Python README](../).

## Description

[Granian](https://github.com/emmett-framework/granian) is an asyncio server for Python applications.

## Test Paths & Source

Granian includes three different implementations:

- ASGI implementation in [app\_asgi.py](app_asgi.py)
- RSGI implementation in [app\_rsgi.py](app_rsgi.py)
- WSGI implementation in [app\_wsgi.py](app_wsgi.py)

ASGI and RSGI implementations includes the following tests:

* JSON Serialization: "/json"
* Plaintext: "/plaintext"
* Single Database Query: "/db"
* Multiple Database Queries: "queries?queries=#"
* Fortunes: "/fortunes"
* Database Updates: "updates?queries=#"

while WSGI implementation only include JSON Serialization and Plaintext tests.

*Replace # with an actual number.*

## Resources

* [Github repository](https://github.com/emmett-framework/granian)
