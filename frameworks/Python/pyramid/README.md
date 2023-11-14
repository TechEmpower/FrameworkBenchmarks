# Pyramid benchmark test

This is the Python Pyramid portion of a [benchmarking tests suite](../../) 
comparing a variety of frameworks.

The information below is specific to Pyramid. For further guidance, 
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki). 
Also note that there is additional information that's provided in 
the [Python README](../).

[Pyramid](http://www.pylonsproject.org/) is a flexible Python 3 framework.
This test uses [SQLAlchemy](http://www.sqlalchemy.org/) as its ORM, the default
[Chameleon](http://www.pylonsproject.org/) for its templating, and
[Gunicorn](https://github.com/benoitc/gunicorn) for the application server.

## Test Paths & Source

* [JSON Serialization](frameworkbenchmarks/tests.py): "/json"
* [Single Database Query](frameworkbenchmarks/tests.py): "/db", [World Model](frameworkbenchmarks/models.py)
* [Multiple Database Queries](frameworkbenchmarks/tests.py): "queries?queries=#"*, [World Model](frameworkbenchmarks/models.py)
* [Fortunes](frameworkbenchmarks/tests.py): "/fortunes", [Fortune Model](frameworkbenchmarks/models.py)
* [Database Updates](frameworkbenchmarks/tests.py): "updates?queries=#"*, [World Model](frameworkbenchmarks/models.py)
* [Plaintext](frameworkbenchmarks/tests.py): "/plaintext"

*Replace # with an actual number.

## Get Help

### Community

* `#pyramid` IRC Channel ([irc.libera.chat](https://libera.chat))
* [Pyramid (pylons-discuss) Google Group](https://groups.google.com/forum/#!forum/pylons-discuss)
