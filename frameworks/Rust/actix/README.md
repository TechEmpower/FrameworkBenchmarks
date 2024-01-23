# [Actix](https://actix.rs) web framework

## Description

Actix web is a small, fast, pragmatic, open source rust web framework.

* [User Guide](https://actix.rs/book/actix-web/)
* [API Documentation](https://docs.rs/actix-web/)
* [Chat on gitter](https://gitter.im/actix/actix)
* Cargo package: [actix-web](https://crates.io/crates/actix-web)

## Features

* Supported HTTP/1.x and HTTP/2.0 protocols
* Streaming and pipelining
* Keep-alive and slow requests handling
* Client/Server WebSockets
* Transparent content compression/decompression (br, gzip, deflate)
* Configurable request routing
* Graceful server shutdown
* Multipart streams
* Middlewares (Logger, Session, DefaultHeaders, CORS)

## Databases

* PostgreSQL
  * Raw driver access via [`tokio_postgres`](https://docs.rs/tokio-postgres/latest/tokio_postgres/) (actix-http test)
  * ORM using [`diesel`](http://diesel.rs) (actix-web-diesel test)
  * Raw driver access via [`tokio_postgres`](https://docs.rs/tokio-postgres/latest/tokio_postgres/) with connection pooling using [`deadpool_postgres`](https://docs.rs/deadpool-postgres/latest/deadpool_postgres/) (actix-web-pg-deadpool test)

* MongoDB
  * Raw driver access and connection pooling via [`mongodb`](https://docs.rs/mongodb/latest/mongodb/) (actix-web-mongodb test)

## Test URLs

### Test 1: JSON Encoding

    http://localhost:8080/json

### Test 2: Single Row Query

    http://localhost:8080/db

### Test 3: Multi Row Query

    http://localhost:8080/queries?q=20

### Test 4: Fortunes (Template rendering)

    http://localhost:8080/fortune

### Test 5: Update Query

    http://localhost:8080/updates?q=20

### Test 6: Plaintext

    http://localhost:8080/plaintext
