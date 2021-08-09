# [Flask](http://flask.pocoo.org/) Benchmark Test

The information below is specific to Flask. For further guidance, 
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki). 
Also note that there is additional information that's provided in 
the [Python README](../).

This is the Python Flask portion of a [benchmarking tests suite](../../) 
comparing a variety of frameworks.

All test implementations are located within a single file 
([app.py](app.py)).

## Description

Flask + Flask-SQLAlchemy

### Database

PostgresQL (psycopg2 on CPython, psycopg2cffi on PyPy)

### Server

* gunicorn+meinheld on CPython
* gunicorn on PyPy

## Test URLs
### JSON Encoding 

http://localhost:8080/json

### Single Row Random Query

With ORM (app.py):
    http://localhost:8080/db

Without ORM (app_raw.py):
    http://localhost:8080/db

### Variable Row Query Test 

With ORM (app.py):
    http://localhost:8080/query?queries=2

Without ORM (app_raw.py):
    http://localhost:8080/query?queries=2
