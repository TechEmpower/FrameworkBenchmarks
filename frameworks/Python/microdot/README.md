# Microdot Benchmarking Test

This is the [Microdot](https://github.com/miguelgrinberg/microdot) portion of a [benchmarking tests suite](../../) comparing a variety of web development platforms.

The information below is specific to Microdot.
For further guidance, review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note that there is additional information provided in the [Python README](../).

### Test Source Code

* [JSON](app_sync.py#L60)
* [JSON-async](app_async.py#L60)
* [PLAINTEXT](app_sync.py#L102)
* [PLAINTEXT-async](app_async.py#L102)
* [DB](app_sync.py#L65)
* [DB-async](app_async.py#L65)
* [QUERY](app_sync.py#L73)
* [QUERY-async](app_async.py#L83)
* [CACHED QUERY](app_sync.py#L112)
* [CACHED_QUERY-async](app_async.py#L112)
* [UPDATE](app_sync.py#L89)
* [UPDATE-async](app_async.py#L89)
* [FORTUNES](app_sync.py#L80)
* [FORTUNES-async](app_async.py#L80)

## Resources

* [Microdot](https://github.com/miguelgrinberg/microdot)
* [Alchemical](https://github.com/miguelgrinberg/alchemical)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/queries?queries=

### CACHED QUERY

http://localhost:8080/cached_query?count=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes
