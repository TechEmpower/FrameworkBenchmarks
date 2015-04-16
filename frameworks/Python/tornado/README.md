# Tornado Benchmarking Test

This is the Tornado portion of a [benchmarking tests suite](../../) 
comparing a variety of web development platforms.

The information below is specific to Tornado. For further guidance, 
review the [documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/). 
Also note that there is additional information that's provided in 
the [Python README](../).

## Infrastructure Software

* CPython 2, 3 and PyPy
* [Tornado](https://www.tornadoweb.com/)
* [Mongodb](https://www.mongodb.org/) with [motor](http://motor.readthedocs.org/en/stable/)
* [PostgreSQL](http://www.postgresql.org/) with [momoko](http://momoko.61924.nl/en/latest/)

## Test Paths & Sources

### Raw Database Tests

* [Single Database Query](server.py): "/dbraw"
* [Multiple Database Queries](server.py): "/queriesraw?queries=#"*

### Tests

* [JSON Serialization](server.py): "/json"
* [Single Database Query](server.py): "/db"
* [Multiple Database Queries](server.py): "/queries?queries=#"*
* [Plaintext](server.py): "/plaintext"

*Replace # with an actual number.

## Get Help

### Community

* [python-tornado Google Group](https://groups.google.com/forum/#!forum/python-tornado)

### Resources

* http://www.tornadoweb.org/en/stable/documentation.html
