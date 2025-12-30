# Ditsmod Benchmarking Test

This is the Ditsmod portion of a benchmarking test suite comparing a variety of web development platforms.

Information about Ditsmod can be found at https://github.com/ditsmod/ditsmod or https://ditsmod.github.io/en/

### Test Type Implementation Source Code

* JSON, PLAINTEXT in [this controller](src/app/modules/routed/simple/without-db.controller.ts)
* DB, QUERY, UPDATE and CACHED QUERY in [this controller](src/app/modules/routed/simple/db.controller.ts)
* FORTUNES in [this controller](src/app/modules/routed/simple/fortune.controller.ts)

## Test URLs
### JSON

http://tfb-server:8080/json

### PLAINTEXT

http://tfb-server:8080/plaintext

### DB

http://tfb-server:8080/db

### QUERY

http://tfb-server:8080/queries?queries=10

### UPDATE

http://tfb-server:8080/updates?queries=10

### CACHED QUERY

http://tfb-server:8080/cached-queries?count=10

### Fortune

http://tfb-server:8080/fortunes