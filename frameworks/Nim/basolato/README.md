# Basolato Benchmarking Test

### Test Type Implementation Source Code

* [JSON](./app/http/controllers/benchmark_controller.nim)
* [PLAINTEXT](./app/http/controllers/benchmark_controller.nim)
* [DB](./app/http/controllers/benchmark_controller.nim)
* [QUERY](./app/http/controllers/benchmark_controller.nim)
* [UPDATE](./app/http/controllers/benchmark_controller.nim)
* [FORTUNES](./app/http/controllers/benchmark_controller.nim)

## Important Libraries
The tests were run with:
* [Software](https://github.com/itsumura-h/nim-basolato)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/queries?queries=1

### UPDATE

http://localhost:8080/updates?queries=1

### FORTUNES

http://localhost:8080/fortunes
