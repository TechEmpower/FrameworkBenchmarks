# warp-rust Benchmarking Test

warp is a composable web server framework based on hyper.

* [API Documentation](https://docs.rs/warp/0.3)

### Test Type Implementation Source Code

* [JSON](src/main.rs)
* [PLAINTEXT](src/main.rs)
* [DB](src/main.rs)
* [QUERIES](src/main.rs)
* [FORTUNES](src/main.rs)
* [UPDATE](src/main.rs)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERIES

http://localhost:8080/queries/[1...500]

### FORTUNES

http://localhost:8080/fortunes
