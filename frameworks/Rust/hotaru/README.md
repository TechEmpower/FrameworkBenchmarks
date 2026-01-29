# Hotaru Framework - TechEmpower Benchmark Implementation

[Hotaru](https://github.com/Field-of-Dreams-Studio/hotaru) is a lightweight, intuitive full-stack web framework for Rust that emphasizes simplicity and developer experience.

## Framework Information

- **Homepage**: https://github.com/Field-of-Dreams-Studio/hotaru
- **Documentation**: https://docs.rs/hotaru
- **Version**: 0.7.7
- **Language**: Rust
- **Platform**: Tokio async runtime

## Implemented Tests

According to the [TechEmpower Framework Benchmarks specification](https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview), this implementation includes all 7 required test types:

### 1. JSON Serialization Test (`/json`)
- **Endpoint**: `GET /json`
- **Response**: `{"message":"Hello, World!"}`
- **Content-Type**: `application/json`
- Tests framework fundamentals including routing, JSON serialization, and response handling

### 2. Plaintext Test (`/plaintext`)
- **Endpoint**: `GET /plaintext`
- **Response**: `Hello, World!`
- **Content-Type**: `text/plain`
- Tests raw request routing and response generation capabilities

### 3. Single Database Query Test (`/db`)
- **Endpoint**: `GET /db`
- **Response**: `{"id": 123, "randomNumber": 456}`
- **Content-Type**: `application/json`
- Fetches one random World row (ID 1-10000) from PostgreSQL

### 4. Multiple Queries Test (`/queries`)
- **Endpoint**: `GET /queries?queries=N`
- **Response**: Array of World objects
- **Constraints**: `queries` parameter clamped to 1-500
- Performs N database queries and returns results as JSON array

### 5. Updates Test (`/updates`)
- **Endpoint**: `GET /updates?queries=N`
- **Response**: Array of updated World objects
- **Constraints**: `queries` parameter clamped to 1-500
- Fetches N random World rows, updates randomNumber field, persists to database

### 6. Cached Queries Test (`/cached-worlds`)
- **Endpoint**: `GET /cached-worlds?count=N`
- **Response**: Array of World objects
- **Constraints**: `count` parameter clamped to 1-500
- **Cache**: In-memory cache (moka) pre-warmed with all 10,000 World rows at startup
- Falls back to database on cache miss

### 7. Fortunes Test (`/fortunes`)
- **Endpoint**: `GET /fortunes`
- **Response**: HTML table of fortunes, sorted by message
- **Content-Type**: `text/html; charset=utf-8`
- Fetches all Fortune rows, adds test fortune, sorts, and renders HTML template

## Technology Stack

- **Web Framework**: Hotaru 0.7.7
- **Template Engine**: Akari (Hotaru's built-in template system)
- **Database**: PostgreSQL via tokio-postgres
- **Connection Pool**: deadpool-postgres
- **Cache**: moka (in-memory, 10,000 entry capacity)
- **Async Runtime**: Tokio

## Running Locally

### Prerequisites
- Rust 1.93.0+
- PostgreSQL database with TechEmpower schema

### Run the Server

```bash
cd frameworks/Rust/hotaru
cargo run --release
```

The server will start on `http://0.0.0.0:8080`

### Environment Variables

```bash
DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@localhost/hello_world
DB_POOL_SIZE=56  # Database connection pool size
```

## Test Endpoints

```bash
# JSON serialization
curl http://localhost:8080/json

# Plaintext
curl http://localhost:8080/plaintext

# Single database query
curl http://localhost:8080/db

# Multiple queries (10 queries)
curl "http://localhost:8080/queries?queries=10"

# Updates (5 updates)
curl "http://localhost:8080/updates?queries=5"

# Cached queries (100 from cache)
curl "http://localhost:8080/cached-worlds?count=100"

# Fortunes (HTML)
curl http://localhost:8080/fortunes
```

## Performance

Benchmark results on TechEmpower infrastructure (512 concurrent connections):

- **Plaintext**: ~290,802 requests/sec
- **JSON**: ~290,802 requests/sec
- **Cached Queries (1)**: ~290,802 requests/sec
- **Cached Queries (20)**: ~191,349 requests/sec
- **Cached Queries (100)**: ~87,395 requests/sec

Average latency: 1.94-6.21ms under heavy load

## Implementation Details

### Architecture

- **Lazy Static App**: Hotaru app initialized once using `Lazy<SApp>`
- **Cache Warm-up**: All 10,000 World records pre-loaded into moka cache at startup
- **Connection Pooling**: deadpool-postgres with configurable pool size (default: 56)
- **Zero-Copy Caching**: Uses `Arc<World>` for efficient cache sharing

### HTTP Headers

All responses include required TechEmpower headers:
- `Server: hotaru`
- `Date: <RFC 7231 timestamp>`
- Appropriate `Content-Type` headers

### Database Schema

```sql
-- World table (10,000 rows, IDs 1-10000)
CREATE TABLE World (
  id integer NOT NULL,
  randomNumber integer NOT NULL,
  PRIMARY KEY (id)
);

-- Fortune table
CREATE TABLE Fortune (
  id integer NOT NULL,
  message varchar(2048) NOT NULL,
  PRIMARY KEY (id)
);
```

## File Structure

```
frameworks/Rust/hotaru/
├── Cargo.toml              # Dependencies and build config
├── README.md               # This file
├── benchmark_config.json   # TFB configuration
├── hotaru.dockerfile       # Docker build instructions
├── setup.py               # TFB setup script
├── src/
│   ├── main.rs            # Hotaru server and endpoints
│   ├── database.rs        # Database pool, cache, and queries
│   ├── models.rs          # Data models (World, Fortune)
│   └── utils.rs           # Helper functions
└── templates/
    └── fortunes_hotaru.html  # Fortune HTML template (Akari syntax)
```

## Notes

- Uses Rust edition 2024
- All dependencies from crates.io (no local path dependencies)
- Follows TechEmpower requirements for header handling, query clamping, and HTML escaping
- Cache implementation uses lazy population on miss
- Prepared statements cached for all database queries

## Links

- **Hotaru Framework**: https://github.com/Field-of-Dreams-Studio/hotaru
- **Documentation**: https://docs.rs/hotaru
- **TechEmpower Benchmarks**: https://www.techempower.com/benchmarks/
