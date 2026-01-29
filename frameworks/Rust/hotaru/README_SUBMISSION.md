# Hotaru TechEmpower Benchmark Submission

This directory contains the TechEmpower Framework Benchmarks submission for Hotaru.

## Files Overview

- `benchmark_config.json` - Main benchmark configuration (updated to R23 schema)
- `hotaru.dockerfile` - Multi-stage Docker build for the benchmark
- `Cargo.toml` - Rust dependencies and project config
- `src/` - Source code with all benchmark endpoints
- `templates/` - Templates for the fortunes test
- `setup.py` - Minimal helper (not used by current toolset)

## Configuration Summary

### Endpoints Implemented
- `/json` - JSON serialization test
- `/plaintext` - Plaintext response test
- `/db` - Single database query
- `/queries?queries=N` - Multiple database queries (1-500)
- `/updates?queries=N` - Multiple database updates (1-500)
- `/cached-worlds?count=N` - Cached query test (1-500)
- `/fortunes` - Fortune cookie test with HTML rendering

### Key Configuration Details
- **Framework**: Hotaru
- **Language**: Rust
- **Platform**: Rust
- **Database**: PostgreSQL
- **ORM**: Raw SQL
- **Port**: 8080
- **Docker Command**: `/app/server`

## Verified Against Local TFB Toolset

### Schema Notes
- `dockerfile` and `docker_cmd` are supported by the toolset (see `toolset/utils/docker_helper.py`).
- `cached_query_url` is the correct key for cached queries (see `toolset/test_types/cached-query/cached-query.py`).
- All required metadata fields are present (approach, classification, database, etc.).

### Parameter Names
- Query tests use `?queries=` (standard TFB convention)
- Cached query uses `?count=` (matches implementation in `src/main.rs`)

## Running Locally with TFB Toolset

1. Clone TechEmpower/FrameworkBenchmarks repo
2. Copy this directory to `frameworks/Rust/hotaru/`
3. Configure `benchmark.cfg` in TFB root:
   ```ini
   client_host=localhost
   client_identity_file=/Users/yourusername/.ssh/id_rsa
   client_user=yourusername

   database_host=localhost
   database_identity_file=/Users/yourusername/.ssh/id_rsa
   database_user=yourusername

   server_host=localhost
   ```
4. Run verification:
   ```bash
   ./toolset/run-tests.py --mode verify --test hotaru
   ```

## Environment Variables

The Dockerfile sets:
- `DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world`
- `DB_POOL_SIZE=56`

These match TFB's standard database configuration.
