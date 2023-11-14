# [Just-JS](https://github.com/just-js) Benchmarking Test

This test benchmarks the [Just-JS](https://github.com/just-js) framework.

Author: Andrew Johnston <billy@billywhizz.io>

## Test Type Implementation Source Code

* [JSON] techempower.js
* [PLAINTEXT] techempower.js
* [DB] techempower.js
* [QUERY] techempower.js
* [CACHED QUERY] techempower.js
* [UPDATE] techempower.js
* [FORTUNES] techempower.js

## Test URLs

### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?q=

### UPDATE

http://localhost:8080/update?q=

### FORTUNES

http://localhost:8080/fortunes

### CACHED QUERY

http://localhost:8080/cached-world?q=

## Building the Docker Image
```bash
docker build -t techempower:latest -f just.dockerfile .
```

## Running the TFB Postgres Docker Container
```bash
## docker network create -d bridge tfb
docker run -p 5432:5432 -d --rm --name tfb-database --network tfb techempower/tfb.database.postgres:latest
```

## Running the Docker Container
```bash
docker run -p 8080:8080 -d --rm --name tfb-server --network tfb techempower:latest
```