# BlackSheep Benchmark Test

This is the BlackSheep portion of a [benchmarking tests suite](../../)
comparing a variety of web development platforms.

The information below is specific to BlackSheep. For further guidance,
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note that there is additional information provided in
the [Python README](../).

## Description

[BlackSheep](https://github.com/RobertoPrevato/BlackSheep) is a fast HTTP Server/Client microframework for Python [asyncio](https://docs.python.org/3/library/asyncio.html), using [Cython](https://cython.org), 
[`uvloop`](https://magic.io/blog/uvloop-blazing-fast-python-networking/), and 
[`httptools`](https://github.com/MagicStack/httptools). 

<p align="left">
  <a href="#blacksheep"><img width="320" height="271" src="https://raw.githubusercontent.com/RobertoPrevato/BlackSheep/master/black-sheep.svg?sanitize=true" alt="Black Sheep"></a>
</p>


## Implementation

BlackSheep is implemented using:

* [asyncio](https://docs.python.org/3/library/asyncio.html).
* [Cython](https://cython.org)
* [`uvloop`](https://magic.io/blog/uvloop-blazing-fast-python-networking/).
* [`httptools`](https://github.com/MagicStack/httptools).
* Python built-in multiprocessing module.

## Test Paths & Sources

All of the test implementations are located within a single file ([app.py](app.py)).

## Resources

* [Repo](https://github.com/RobertoPrevato/BlackSheep)
