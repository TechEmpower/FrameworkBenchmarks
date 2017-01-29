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

### JSON Encoding 

http://localhost:8000/json

### Single Row Random Query

With ORM:
    http://localhost:8000/dbs

Without ORM (raw):
    http://localhost:8000/dbsraw

### Variable Row Query Test 

With ORM:
    http://localhost:8000/db?queries=2

Without ORM (raw):
    http://localhost:8000/dbraw?queries=2
