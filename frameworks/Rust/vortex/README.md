# Vortex

High-performance HTTP server built for TechEmpower benchmarks.

## Architecture

- **I/O**: io_uring with multishot accept, DEFER_TASKRUN, cascading kernel fallbacks
- **Threading**: Thread-per-core with CPU pinning (no work-stealing)
- **HTTP**: Tiered parser — single-byte route classification for benchmark paths
- **JSON**: Hand-optimized direct-to-buffer serialization (no serde)
- **Database**: Custom PostgreSQL binary wire protocol with prepared statements and pipelined queries
- **Memory**: mimalloc global allocator, pre-allocated per-worker buffers, BufReader on DB connections
- **Compilation**: fat LTO, codegen-units=1, target-cpu=native, panic=abort

## Test URLs

- `/plaintext` - Plaintext response
- `/json` - JSON serialization
- `/db` - Single database query
- `/queries?q=` - Multiple database queries
- `/fortunes` - Fortunes HTML template
- `/updates?q=` - Database updates
