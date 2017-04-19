# API Star Benchmark Test

This is the API Star portion of a [benchmarking tests suite](../../)
comparing a variety of web development platforms.

The information below is specific to API Star. For further guidance,
review the [documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/).
Also note that there is additional information provided in
the [Python README](../).

## Description

[API Star](https://github.com/tomchristie/apistar) is a fast and expressive
API framework for Python 3.

## Server

* Gunicorn & Meinheld on Python 3

## Test Paths & Sources

All of the test implementations are located within a single file ([app.py](app.py)).

* [JSON Serialization](app.py): "/json"
* [Plaintext](app.py): "/plaintext"

## Resources

* [Repo](https://github.com/tomchristie/apistar)
