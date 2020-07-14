# FastAPI Benchmarking Test

This is the FastAPI portion of a [benchmarking tests suite](../../)
comparing a variety of web development platforms.

The information below is specific to FastAPI. For further guidance,
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note that there is additional information provided in
the [Python README](../).

## Description

[**FastAPI**](https://github.com/tiangolo/fastapi) is a modern, fast (high-performance), web framework for building APIs with Python 3.6+.

The key features are:

* **Fast**: Very high performance, on par with **NodeJS** and **Go** (thanks to Starlette and Pydantic).

* **Fast to code**: Increase the speed to develop features by about 200% to 300% *.
* **Less bugs**: Reduce about 40% of human (developer) induced errors. *
* **Intuitive**: Great editor support. <abbr title="also known as auto-complete, autocompletion, IntelliSense">Completion</abbr> everywhere. Less time debugging.
* **Easy**: Designed to be easy to use and learn. Less time reading docs.
* **Short**: Minimize code duplication. Multiple features from each parameter declaration. Less bugs.
* **Robust**: Get production-ready code. With automatic interactive documentation.
* **Standards-based**: Based on (and fully compatible with) the open standards for APIs: <a href="https://github.com/OAI/OpenAPI-Specification" target="_blank">OpenAPI</a> and <a href="http://json-schema.org/" target="_blank">JSON Schema</a>.

<small>* estimation based on tests on an internal development team, building production applications.</small>

## Test Paths & Sources

All of the test implementations are located within a single file ([app.py](app.py)).

All the tests are based on the ones for Starlette, as FastAPI is basically Starlette on steroids plus Pydantic, with many features specifically desgined for API development. All this while still supporting all the other features provided by Starlette.

## Resources

* [FastAPI source code on GitHub](https://github.com/tiangolo/fastapi)
* [FastAPI website - documentation](https://fastapi.tiangolo.com)
