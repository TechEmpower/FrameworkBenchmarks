# [hyperlane](https://github.com/ltpp-universe/hyperlane) web framework

## Description

Hyperlane is a lightweight and high-performance Rust HTTP server library designed to simplify network service development. It supports HTTP request parsing, response building, TCP communication, and redirection features, making it ideal for building modern web services.

## Database

PostgreSQL.

- url: postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world
- tfb-database read from env DBHOST

## Docker file

```sh
docker build -t hyperlane_techempower -f hyperlane.dockerfile .;
# Replace DBHOST with the database host address
docker run --name hyperlane_techempower --network=host -e DBHOST=127.0.0.1 -d hyperlane_techempower;
```

## Test URLs

### Test 1: JSON Encoding

    http://localhost:60000/json

### Test 2: Single Row Query

    http://localhost:60000/db

### Test 3: Multi Row Query

    http://localhost:60000/queries?queries=20

### Test 4: Plaintext

    http://localhost:60000/plaintext
