# Falcon Benchmark Test

This is the Falcon portion of a [benchmarking tests suite](../../) 
comparing a variety of web development platforms.

The information below is specific to Falcon. For further guidance, 
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki). 
Also note that there is additional information provided in 
the [Python README](../).

## Description

[**Falcon**](https://falconframework.org/) is a blazing fast, minimalist Python web API framework for building robust app backends and microservices. The framework works great with both asyncio (ASGI) and gevent/meinheld (WSGI).

Features:
* ASGI, WSGI, and WebSocket support
Native asyncio support.
* No reliance on magic globals for routing and state management.
* Stable interfaces with an emphasis on backwards-compatibility.
* Simple API modeling through centralized RESTful routing.
* Highly-optimized, extensible code base.
* DRY request processing via middleware components and hooks.
* Strict adherence to RFCs.
* Idiomatic HTTP error responses.
* Straightforward exception handling.
* WSGI/ASGI testing helpers and mocks.
* CPython 3.5+ and PyPy 3.5+ support.

## Infrastructure Software

### Server

* Waitress on CPython3
* Bjoern on Cpython3
* Gunicorn + Meinheld on CPython3
* Gunicorn + Meinheld with Orjson on CPython3
* Gunicorn Sync on PyPy3

### Database

* Pony ORM [PostgreSQL] - (psycopg2-binary on CPython3, psycopg2cffi on PyPy3)

## Test Paths & Sources

All of the test implementations are located within a single file ([app.py](app.py)) and accessible via http://localhost:8080.

* [JSON Serialization](app.py): "/json"
* [Single Database Query](app.py): "/db"
* [Multiple Database Queries](app.py): "/queries/"
* [Fortunes](app.py): "/fortunes"
* [Database Updates](app.py): "/updates/"
* [Plaintext](app.py): "/plaintext"

## Get Help

### Resources

* [Falcon on Github](https://github.com/falconry/falcon)
* [Docs](https://falcon.readthedocs.io/en/stable/)
* [FAQ](https://falcon.readthedocs.io/en/stable/user/faq.html#faq)
* [Wiki](https://github.com/falconry/falcon/wiki)

### Community

* Chatrooms [Falcon for Users](https://gitter.im/falconry/user) and [Falcon for Contributors](https://gitter.im/falconry/dev) @ Gitter
* [Submit an issue](https://github.com/falconry/falcon/issues)
