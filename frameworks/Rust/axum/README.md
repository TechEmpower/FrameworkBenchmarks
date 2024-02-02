# [Axum](https://github.com/tokio-rs/axum)

## Description

Axum is a web application framework that focuses on ergonomics and modularity,
built with Tokio, Tower, and Hyper.

- [User Guide](https://docs.rs/axum/latest/axum/)
- [API Documentation](https://docs.rs/axum/latest/axum/)
- [Cargo Package (`axum`)](https://crates.io/crates/axum)

## Variants

- PostgreSQL using `SQLx`, `tokio_postgres`, and `deadpool`.
- MongoDB with `mongodb`.

## Test URLs

- Plaintext: http://localhost:8000/plaintext
- JSON Encoding: http://localhost:8000/json
- Single Row Query: http://localhost:8000/db
- Multi Row Query: http://localhost:8000/queries?q=20
- Fortunes: http://localhost:8000/fortunes
- Update Query: http://localhost:8000/updates?q=20
- Cached Query: http://localhost:8000/cached-queries?q=20

## Notable Points (both performance and build)

- Use of `async`.
- Use of most recent versions of Rust, `axum` and dependencies.
- (Disabled by default) Compile-time swap-in of `simd-json` instead of `serde_json` for faster JSON serialization.
- Release binaries are stripped and compiled with CPU native.
- Sockets configured with TCP_NODELAY and to support an increased number of pending connections.
- Server configured to serve HTTP/1 only, with no need for websockets.
- Separation of build and deployment containers using multi-stage builds.
- Deployment into Google's minimal `distroless-cc` container.
- Use of pipelined database queries (where supported).
- Streaming database queries (where supported).
- Use of PostgreSQL prepared statements cache (where supported).
- Use of PostgreSQL arrays to execute multi-row database updates with a single `UPDATE` query.
  - This is permitted by the [test requirements](https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#database-updates), step (ix).
- In version 0.7.6 (as yet unreleased), a native API to set TCP_NODELAY will be included.
  - https://github.com/tokio-rs/axum/pull/2653/
  - https://github.com/tokio-rs/axum/issues/2521
- More performance improvements are to be expected in version 0.8:
  - https://github.com/tokio-rs/axum/issues/1827
