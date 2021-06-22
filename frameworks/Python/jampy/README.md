# [Jam.py](http://jam-py.com/) Benchmarking Test

This is the Jam.py portion of a [benchmarking tests suite](../../)
comparing a variety of web development platforms.

The information below is specific to Jam.py. For further guidance,
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note that there is additional information provided in
the [Python README](../).


## Web Server and Database Client Software

* [Meinheld](http://meinheld.org/) 0.5.9
* [Gunicorn](http://gunicorn.org/) 19.4.5
* [greenlet](http://greenlet.readthedocs.io/en/latest/) 0.4.9

## Test Paths

"Jam.py" test is accessed via the "/".

* JSON Serialization: "/standard/json"
* Single Database Query: "/standard/db"
* Multiple Database Queries: "/standard/dbs?queries=#"
* Fortunes: "/"
* Database Updates: "/standard/update?queries=#"
* Plaintext: "/"

*Replace # with an actual number.

## Source Code

## Get Help

* [Jam.py-users Google Group](https://groups.google.com/g/jam-py)

### [Resources](https://jam-py.com/docs/)

* [*Jam.py: Complete Reference Manual*](http://jam-py.com/docs/contents.html)
