# Flask Benchmark Test

Single file test, [app.py](app.py)

## Description

Flask + Flask-SQLAlchemy

### Interpreter

* CPython 2.7.4
* PyPy 2.0

### Database

MySQL (MySQL-python on CPython, PyMySQL on PyPy)

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
