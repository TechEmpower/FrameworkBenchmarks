# Chopin — TechEmpower Framework Benchmarks

[Chopin](https://crates.io/crates/chopin-core) is a high-level Rust web framework built on Axum and Hyper.

This benchmark uses `chopin-core` v0.3.3 with the `perf` feature:
- **FastRoute** for `/json` and `/plaintext` — zero-alloc pre-computed responses (~35ns/req)
- **sonic-rs** SIMD-accelerated JSON serialization for all DB responses via `chopin_core::json::to_bytes()`
- **mimalloc** high-performance allocator (via `perf` feature)
- **start_multicore** — per-core `current_thread` tokio runtime with `SO_REUSEPORT`
- **ChopinService** — FastRoute match bypasses Axum middleware entirely
- **tokio-postgres** raw driver for database queries

## Test URLs

| Test | URL |
|------|-----|
| JSON | /json |
| Plaintext | /plaintext |
| Single query | /db |
| Multiple queries | /queries?q= |
| Fortunes | /fortunes |
| Updates | /updates?q= |
| Cached queries | /cached-queries?q= |

