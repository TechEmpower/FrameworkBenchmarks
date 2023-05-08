
# [Rocket](https://rocket.rs/) - Simple, Fast, Type-Safe Web Framework for Rust

## Description

Rocket is a web framework for Rust that makes it simple to write fast web applications without sacrificing flexibility or type safety. All with minimal code.

* [User Guide](https://rocket.rs/guide/)
* [API Documentation](https://api.rocket.rs/v0.5-rc/rocket/)
* Cargo package: [rocket](https://crates.io/crates/rocket)

## Database

PostgreSQL

* ORM using [diesel](http://diesel.rs)

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
