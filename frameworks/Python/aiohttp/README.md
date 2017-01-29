# [aiohttp](http://aiohttp.readthedocs.io/) Benchmark Test

The information below is specific to aiohttp. For further guidance, 
review the [documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/). 
Also note that there is additional information that's provided in 
the [Python README](../).

This is the Python aiohttp portion of a [benchmarking tests suite](../../) 
comparing a variety of frameworks.

All test implementations are located within ([./app](app)).

## Description

aiohttp with [aiopg + sqlalchemy](http://aiopg.readthedocs.io/en/stable/sa.html) and 
separately [asyncpg](https://magicstack.github.io/asyncpg/current/) for database access.
 
[uvloop](https://github.com/MagicStack/uvloop) is used for a more performant event loop.

### Database

PostgreSQL

### Server

gunicorn+uvloop on CPython

## Test URLs

### Test 1: JSON Encoding 

    http://localhost:8080/json

### Test 2: Single Row Query

With ORM:

    http://localhost:8080/db

Without ORM (raw):

    http://localhost:8080/raw/db

### Test 3: Multi Row Query 

With ORM:

    http://localhost:8080/queries?queries=20

Without ORM (raw):

    http://localhost:8080/raw/queries?queries=20

### Test 4: Fortunes (Template rendering)

With ORM:

    http://localhost:8080/fortunes

Without ORM (raw):

    http://localhost:8080/raw/fortunes

### Test 5: Update Query

With ORM:

    http://localhost:8080/updates?queries=20

Without ORM (raw):

    http://localhost:8080/raw/updates?queries=20

### Test6: Plaintext

    http://localhost:8080/plaintext
