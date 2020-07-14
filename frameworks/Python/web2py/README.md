# [web2py](http://www.web2py.com/) Benchmarking Test

This is the web2py portion of a [benchmarking tests suite](../../)
comparing a variety of web development platforms.

The information below is specific to web2py. For further guidance,
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note that there is additional information provided in
the [Python README](../).

There are two sets of web2py tests, "web2py-standard" (the default) and "web2py-optimized". The former set is implemented via standard web2py production code. The latter involves several special optimizations that would not be typical of a web2py application but could be implemented in cases where performance is critical.

## Web Server and Database Client Software

* [Meinheld](http://meinheld.org/) 0.5.9
* [Gunicorn](http://gunicorn.org/) 19.4.5
* [greenlet](http://greenlet.readthedocs.io/en/latest/) 0.4.9
* [MySQLdb](https://mysqlclient.readthedocs.io/en/latest/) 1.3.7

## Test Paths

"web2py-standard" and "web2py-optimized" tests are accessed via the "/standard" and "/optimized" paths, respectively.

* JSON Serialization: "/standard/json", "/optimized/json"
* Single Database Query: "/standard/db", "/optimized/db"
* Multiple Database Queries: "/standard/dbs?queries=#", "/optimized/dbs?queries=#"*
* Fortunes: "/standard/fortunes", "/optimized/fortunes"
* Database Updates: "/standard/update?queries=#", "/optimized/update?queries=#"*
* Plaintext: "/standard/plaintext", "/optimized/plaintext"

*Replace # with an actual number.

## Source Code

* [Database connection and models](app/standard/modules/database.py)
* [Controller](app/standard/modules/controller.py)
* [Fortunes template](app/standard/views/fortune.html)

## Get Help

### [Community](http://web2py.com/init/default/documentation)

* [web2py-users Google Group](https://groups.google.com/forum/#!forum/web2py)
* [web2py-developers Google Group](https://groups.google.com/forum/#!forum/web2py-developers)

### [Resources](http://web2py.com/init/default/documentation)

* [*web2py: Complete Reference Manual*](http://web2py.com/book)
