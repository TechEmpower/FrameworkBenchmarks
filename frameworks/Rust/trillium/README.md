# [Trillium](https://trillium.rs) web framework

## Description

Trillium is a modular toolkit for building async rust web applications. Trillium runs on stable rust, is fully async, and can run on tokio, async-std, or smol.

* [User Guide](https://trillium.rs/)
* [API Documentation](https://docs.trillium.rs)

## Features

* Trillium includes minimum functionality by default, but offers powerful and composable features as additional crates
* Fully-async HTTP/1.x implementation including pipelining, keep-alive, upgrades, and streaming bodies
* Native-tls and Rustls support available
* Reverse proxy and integrated http client available
* Several template engine integrations available (askama, tera, handlebars, ructe)
* Sessions support available, with multiple session stores (redis, postgres, mysql)
* WebSockets and Server-Sent-Event support available
* Channels feature available
* Body compression available
* Configurable request routing
* Graceful server shutdown


## Database

PostgreSQL.

* ORM using [SeaORM](https://www.sea-ql.org/SeaORM/)

## Test URLs

### Test 1: JSON Encoding

    http://localhost:8080/json

### Test 2: Single Row Query

    http://localhost:8080/db

### Test 3: Multi Row Query

    http://localhost:8080/queries/20

### Test 4: Fortunes (Template rendering)

    http://localhost:8080/fortune

### Test 5: Update Query

    http://localhost:8080/updates/20

### Test 6: Plaintext

    http://localhost:8080/plaintext
