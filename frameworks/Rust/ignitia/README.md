# Ignitia TechEmpower Benchmarks

This repository contains the TechEmpower FrameworkBenchmarks implementations for the Ignitia web framework, designed to showcase high-performance web API serving in Rust.

---

## Project Variants

| Binary           | Description                                       |
|------------------|-------------------------------------------------|
| `ignitia`        | Basic variant with JSON and plaintext endpoints  |
| `ignitia-pg`     | PostgreSQL variant using tokio-postgres          |
| `ignitia-pg-pool`| PostgreSQL variant using deadpool connection pooling |
| `ignitia-mongo`  | MongoDB variant                                  |

---

## Benchmarked Endpoints

All variants implement the following HTTP endpoints:

- `/plaintext` - Returns a simple plaintext "Hello, World!"
- `/json` - Returns a JSON-serialized message
- `/db` - Single database query for a random World record
- `/queries?q=N` - Multiple database queries, default 1, max 500
- `/updates?q=N` - Database updates with N queries, default 1, max 500
- `/fortunes` - Renders HTML table with fortunes and adds one dynamically

---

## Getting Started

### Prerequisites

- Rust (latest stable)
- Docker & Docker Compose (for database services)
- PostgreSQL or MongoDB instance running (Docker recommended)

### Setup Databases

Use the provided `docker-compose.yml` to start PostgreSQL and MongoDB databases locally:

```
docker-compose up -d
```

Initialize databases using provided SQL and JS scripts (`init-db.sql`, `init-mongo.js`).

Update `.env` with correct connection URLs and ports.

### Build and Run

Build all variants:

```
cargo build --release
```

Run a variant (example PostgreSQL):

```
./target/release/ignitia-pg
```

Change ports if needed by modifying `.env`.

### Test APIs

Use curl or the included test scripts to verify endpoints.

Example:

```
curl http://localhost:8000/json
curl http://localhost:8000/queries?q=5
curl http://localhost:8000/fortunes
```

---

## Performance

Ignitia includes several performance optimizations:

- Zero-copy response construction
- Efficient routing with a radix tree
- High-performance async runtime using Tokio
- Connection pooling (deadpool for PostgreSQL)
- MiMalloc memory allocator for improved throughput
- HTTP/2 support and TCP tuning

The expected throughput matches or exceeds other Rust web frameworks in TechEmpower benchmarks.

---

## Docker Support

Build the container image:

```
docker build -t ignitia-techempower -f ignitia.dockerfile .
```

Run the container (default JSON/plaintext variant):

```
docker run -p 8000:8000 ignitia-techempower
```

Override command to run other variants:

```
docker run -p 8000:8000 ignitia-techempower /app/ignitia-pg
```

---

## Contributing

Contributions, bug reports, and feature requests are welcome.

Please submit issues or pull requests following the project coding and style guidelines.

---

## License

This project is licensed under MIT License.

---

## Acknowledgements

- Inspired by the TechEmpower Framework Benchmarks project
- Thanks to the Rust community and authors of Ignitia framework
