# MrHTTP Benchmark Test

This is the MrHTTP portion of a [benchmarking tests suite](../../)
comparing a variety of web development platforms.

The information below is specific to MrHTTP. For further guidance,
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note that there is additional information provided in
the [Python README](../).

## Description

[MrHTTP](https://github.com/MarkReedZ/mrhttp) is an asynchronous web framework for python 3.5+ written in C that has hit 8.5 million requests per second.

## Test Paths & Sources

All of the test implementations are located within a single file ([app.py](app.py)).

* [JSON Serialization](app.py): "/json"
* [Plaintext](app.py): "/plaintext"
