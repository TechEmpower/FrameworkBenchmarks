# [Spyne](http://spyne.io/) Benchmark Test

This is the Python Spyne portion of a [benchmarking tests suite](../../)
comparing a variety of frameworks.

The latest version is at https://github.com/arskom/spyne/tree/master/examples/tfb

All test implementations are located within ([app.py](app.py))

## Description

Spyne + SQLAlchemy

### Database

PostgreSQL (psycopg2 on CPython)

### Server

* gunicorn+wsgi on CPython

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
