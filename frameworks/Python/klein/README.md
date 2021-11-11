# Twisted Klein Benchmark Test

This is the Klein portion of a [benchmarking tests suite](../../) 
comparing a variety of web development platforms.

The information below is specific to Klein. For further guidance, 
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki). 
Also note that there is additional information provided in 
the [Python README](../).

## Description

Klein framework (https://github.com/twisted/klein)

## Infrastructure Software

### Server

* gunicorn+meinheld on CPython
* Tornado on PyPy

## Test Paths & Sources

All of the test implementations are located within a single file ([app.py](app.py)).

* [JSON Serialization](app.py): "/json"
* [Single Database Query](app.py): "/db"
* [Multiple Database Queries](app.py): "/queries?queries=5"
* [Fortunes](app.py): "/fortune"
* [Database Updates](app.py): "/updates?queries=5"
* [Plaintext](app.py): "/plaintext"

## Get Help

### Resources

* [Klein Source Code](https://github.com/twisted/klein)

