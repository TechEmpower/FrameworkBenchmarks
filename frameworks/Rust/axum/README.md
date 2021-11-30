
# [Axum](https://github.com/tokio-rs/axum) - Ergonomic and modular web framework built with Tokio, Tower, and Hyper

## Description

Axum is a web application framework that focuses on ergonomics and modularity.

* [User Guide](https://docs.rs/axum/0.3/axum/)
* [API Documentation](https://docs.rs/axum/0.3/axum/)
* Cargo package: [axum](https://crates.io/crates/axum)

## Database

PostgreSQL

* Raw using [sqlx](https://github.com/launchbadge/sqlx)

## Test URLs

### Test 1: JSON Encoding

    http://localhost:8000/json

### Test 2: Single Row Query

    http://localhost:8000/db

### Test 3: Multi Row Query

    http://localhost:8000/queries?q=20

### Test 4: Fortunes (Template rendering)

    http://localhost:8000/fortunes

### Test 5: Update Query

    http://localhost:8000/updates?q=20

### Test 6: Plaintext

    http://localhost:8000/plaintext
