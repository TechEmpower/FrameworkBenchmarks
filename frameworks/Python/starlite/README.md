# Starlite Benchmarking Test

This is the Starlite portion of a [benchmarking tests suite](../../)
comparing a variety of web development platforms.

The information below is specific to Starlite. For further guidance,
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note that there is additional information provided in
the [Python README](../).

## Description# Starlite

Starlite is a light, opinionated and flexible ASGI API framework built on top
of [pydantic](https://github.com/samuelcolvin/pydantic) and [Starlette](https://github.com/encode/starlette).

## Core Features

* 👉 Class based controllers
* 👉 Decorators based configuration
* 👉 Extended testing support
* 👉 Extensive typing support including inference, validation and parsing
* 👉 Full async (ASGI) support
* 👉 Layered dependency injection
* 👉 OpenAPI 3.1 schema generation with [Redoc](https://github.com/Redocly/redoc) UI
* 👉 Route guards based authorization
* 👉 Simple middleware and authentication
* 👉 Support for pydantic models and pydantic dataclasses
* 👉 Support for standard library dataclasses
* 👉 Ultra-fast json serialization and deserialization using [orjson](https://github.com/ijl/orjson)

## Test Paths & Sources

The API is implemented in a single file ([app.py](app.py)). This Test is based on the Starlette tests.

## Resources

* [Starlite Documentation 📚](https://starlite-api.github.io/starlite/)
* [Starlite Repository ](https://github.com/starlite-api/starlite)