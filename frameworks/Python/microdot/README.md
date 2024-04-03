# Microdot Benchmarking Test

This is the [Microdot](https://github.com/miguelgrinberg/microdot) portion of a [benchmarking tests suite](../../) comparing a variety of web development platforms.

The information below is specific to Microdot.
For further guidance, review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note that there is additional information provided in the [Python README](../).

### Test Source Code

* [JSON](app.py#L60)
* [PLAINTEXT](app.py#L102)
* [DB](app.py#L65)
* [QUERY](app.py#L73)
* [CACHED QUERY](app.py#L112)
* [UPDATE](app.py#L89)
* [FORTUNES](app.py#L80)

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
