# [Morepath](http://morepath.readthedocs.io/) Benchmark Test

The information below is specific to Morepath. For further guidance,
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note that there is additional information that's provided in
the [Python README](../).

This is the Python Morepath portion of a [benchmarking tests suite](../../)
comparing a variety of frameworks.

All test implementations are located within ([./app](app)).

## Description

Morepath with [PonyOrm](https://docs.ponyorm.com/) using PostgreSQL for
database access.

### Database

PostgreSQL (with PonyORM).

### Server

gunicorn + meinheld on CPython

## Test URLs

### Test 1: JSON Encoding

    http://localhost:8080/json

### Test 2: Single Row Query

    http://localhost:8080/db

### Test 3: Multi Row Query

    http://localhost:8080/queries?queries=20

### Test 4: Fortunes (Template rendering)

    http://localhost:8080/fortunes

### Test 5: Update Query

    http://localhost:8080/updates?queries=20

### Test 6: Plaintext

    http://localhost:8080/plaintext
