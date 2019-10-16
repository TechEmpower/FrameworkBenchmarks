# APIDaora Benchmarking Test

This is the APIDaora portion of a [benchmarking tests suite](../../)
comparing a variety of web development platforms.

The information below is specific to APIDaora. For further guidance,
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note that there is additional information provided in
the [Python README](../).

## Description

[**APIDaora**](https://github.com/dutradda/apidaora) is a HTTP/REST API using <b>dataclasses</b> and <b>TypedDict</b> annotation for python3.8+.

## Test Paths & Sources

The default test implementations are located within the file ([app.py](app.py)).
The core module test implementations are located within the file ([coreapp.py](coreapp.py)).

All the tests are based on the ones for FastAPI, as APIDaora is an asgi application and have the same principles of using typing annotations for validation/serialization of data.

## Resources

* [APIDaora source code on GitHub](https://github.com/dutradda/apidaora)
* [APIDaora website - documentation](https://dutradda.github.io/apidaora/)
