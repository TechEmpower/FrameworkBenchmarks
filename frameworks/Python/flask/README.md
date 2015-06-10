# [Flask](http://flask.pocoo.org/) Benchmark Test

The information below is specific to Flask. For further guidance, 
review the [documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/). 
Also note that there is additional information that's provided in 
the [Python README](../).

This is the Python Flask portion of a [benchmarking tests suite](../../) 
comparing a variety of frameworks.

All test implementations are located within a single file 
([app.py](app.py)).

## Description

Flask + Flask-SQLAlchemy

### Database

MySQL (mysqlclient on CPython, PyMySQL on PyPy)

### Server

* gunicorn+meinheld on CPython
* Tornado on PyPy

## Test URLs
### JSON Encoding 

http://localhost:8080/json

### Single Row Random Query

With ORM:
    http://localhost:8080/dbs

Without ORM (raw):
    http://localhost:8080/dbsraw

### Variable Row Query Test 

With ORM:
    http://localhost:8080/db?queries=2

Without ORM (raw):
    http://localhost:8080/dbraw?queries=2
