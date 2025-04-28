# [Rama](https://github.com/plabayo/rama)

## Description

Rama (ラマ) is a modular service framework for the Rust language
to move and transform your network packets.

- [User Guide](https://ramaproxy.org/book/preface.html)
- [API Documentation](https://docs.rs/rama/latest/rama/)
- [Cargo Package (`rama`)](https://crates.io/crates/rama)

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
- Use of the most recent versions of Rust, `rama` and dependencies.
- (Disabled by default) Compile-time swap-in of `simd-json` instead of `serde_json` for faster JSON serialization.
- Release binaries are stripped and compiled with CPU native.
- Sockets configured with `TCP_NODELAY` and to support an increased number of pending connections.
- For very simple benchmarks, use of a separate, single-threaded Tokio runtime for each thread.
- Server configured to serve HTTP/1 only, with no need for websockets.
- Separation of build and deployment containers using multi-stage builds.
- Deployment into Google's minimal `distroless-cc` container.
- Use of pipelined database queries (where supported).
- Streaming database queries (where supported).
- Use of PostgreSQL prepared statements cache (where supported).
- Use of PostgreSQL arrays to execute multi-row database updates with a single `UPDATE` query.
  - This is permitted by the [test requirements](https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#database-updates), step (ix).
- Use of a fast PRNG
