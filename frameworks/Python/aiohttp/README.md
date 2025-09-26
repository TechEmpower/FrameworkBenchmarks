# [aiohttp](http://aiohttp.readthedocs.io/) Benchmark Test

The information below is specific to aiohttp. For further guidance, 
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki). 
Also note that there is additional information that's provided in 
the [Python README](../).

This is the Python aiohttp portion of a [benchmarking tests suite](../../) 
comparing a variety of frameworks.

All test implementations are located within ([./app](app)).

## Description

aiohttp with [sqlalchemy](https://docs.sqlalchemy.org/en/14/orm/extensions/asyncio.html) and
separately [asyncpg](https://magicstack.github.io/asyncpg/current/) for database access.
 
[uvloop](https://github.com/MagicStack/uvloop) is used for a more performant event loop.

### Database

PostgreSQL.

Two variants:
* ORM using [sqlalchemy](https://docs.sqlalchemy.org/en/14/orm/extensions/asyncio.html)
* RAW using [asyncpg](https://magicstack.github.io/asyncpg/current/)

**To enabled "RAW" mode set the following environment variable:**
 
```
export CONNECTION=RAW
```

This will switch which database engine the app uses to execute queries with tests 2, 3, 4 & 5.

### Server

gunicorn+uvloop on CPython

## Test URLs

### Test 1: JSON Encoding 

    http://localhost:8080/json

### Test 2: Single Row Query

    http://localhost:8080/db

### Test 3: Multi Row Query 

    http://localhost:8080/queries/20

### Test 4: Fortunes (Template rendering)

    http://localhost:8080/fortunes

### Test 5: Update Query

    http://localhost:8080/updates/20

### Test 6: Plaintext

    http://localhost:8080/plaintext
