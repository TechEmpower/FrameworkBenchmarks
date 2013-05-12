# Flask Benchmark Test

Single file test, [app.py](app.py)


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

### Fortune Test

With ORM:
    http://localhost:8080/fortune

Without ORM (raw):
    http://localhost:8080/fortuneraw


