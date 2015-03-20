# Falcon Benchmark Test (ported from Flask example)

This is the Falcon portion of a [benchmarking tests suite](../../) 
comparing a variety of web development platforms.

The information below is specific to Falcon. For further guidance, 
review the [documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/). 
Also note that there is additional information provided in 
the [Python README](../).

## Description

Falcon API framework (http://falconframework.org)

## Infrastructure Software

### Server

* gunicorn+meinheld on CPython
* Tornado on PyPy

## Test Paths & Sources

All of the test implementations are located within a single file ([app.py](app.py)).

* [JSON Serialization](app.py): "/json"
* _Single Database Query: N/A_
* _Multiple Database Queries: N/A_
* _Fortunes: N/A_
* _Database Updates: N/A_
* [Plaintext](app.py): "/plaintext"

## Get Help

### Resources

* [Falcon Source Code](https://github.com/falconry/falcon)

### [Community](http://falcon.readthedocs.org/en/0.2.0/community/index.html)

* `#falconframework` IRC Channel ([irc.freenode.net](https://freenode.net/))
* Subscribe to email list by emailing falcon[at]librelist.com and 
following the instructions in the reply.
