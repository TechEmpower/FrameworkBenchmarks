# [LiteSpeed](https://github.com/falconraptor/LiteSpeed/) Benchmarking Test

The information below is specific to Flask. For further guidance, 
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki). 
Also note that there is additional information that's provided in 
the [Python README](../).

This is the Python Flask portion of a [benchmarking tests suite](../../) 
comparing a variety of frameworks.

All test implementations are located within a single file 
([app.py](app.py)).

## Important Libraries
### Database

* MySQL (pymysql + pymysql-pool)

## Test URLs
### JSON

http://localhost:8000/json/

### PLAINTEXT

http://localhost:8000/plaintext/

### DB

http://localhost:8000/db/

### QUERY

http://localhost:8000/query/?queries=

### UPDATE

http://localhost:8000/update/?queries=

### FORTUNES

http://localhost:8000/fortunes/
