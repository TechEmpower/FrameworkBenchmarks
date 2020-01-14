# Saphir Benchmarking Test

Saphir is a fast and lightweight web framework that aims to give lowlevel control over your web stack without the pain of re-doing everything from scratch.

* [Repo](https://github.com/richerarc/saphir)
* [API Documentation](https://docs.rs/saphir/)
* Cargo package: [saphir](https://crates.io/crates/saphir)

### Test Type Implementation Source Code

* [JSON](src/main.rs)
* [PLAINTEXT](src/main.rs)
* [DB](src/main.rs)
* [QUERY](src/main.rs)
* [CACHED QUERY](src/main.rs)
* [UPDATE](src/main.rs)
* [FORTUNES](src/main.rs)

## Important Libraries
The tests were run with:
* [serde](https://github.com/serde-rs/serde)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### ~~DB~~

~~http://localhost:8080/db~~

### ~~QUERY~~

~~http://localhost:8080/query?queries=~~

### ~~CACHED QUERY~~

~~http://localhost:8080/cached_query?queries=~~

### ~~UPDATE~~

~~http://localhost:8080/update?queries=~~

### ~~FORTUNES~~

~~http://localhost:8080/fortunes~~
