# [Quart](https://gitlab.com/pgjones/quart) Benchmarking Test

This benchmark uses Quart with the default Hypercorn server, and asyncpg for database connectivity
(because there is still no good asyncio ORM, sadly).

All code is contained in [app.py](app.py), and should be fairly self-documenting.

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes
