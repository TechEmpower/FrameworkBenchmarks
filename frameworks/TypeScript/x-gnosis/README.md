# x-gnosis

[x-gnosis](https://github.com/affectively-ai/x-gnosis) is an nginx-config-compatible web server built on Aeon Flow topology scheduling. Every layer of request processing uses fork/race/fold primitives:

- **File resolution**: `race(cache, mmap, disk)` -- first to complete wins
- **Compression**: Per-chunk codec racing (identity/gzip/brotli/deflate, smallest wins)
- **Transport**: 10-byte Aeon Flow frames (3x less overhead than HTTP/3)

For these TechEmpower benchmarks, x-gnosis uses Node.js 22 with `node:http` and `cluster` for multi-process spawning.

## Test URLs

- Plaintext: `http://localhost:8080/plaintext`
- JSON: `http://localhost:8080/json`
- Single DB: `http://localhost:8080/db`
- Multiple queries: `http://localhost:8080/queries?queries=20`
- Updates: `http://localhost:8080/updates?queries=20`
- Fortunes: `http://localhost:8080/fortunes`
- Cached queries: `http://localhost:8080/cached-queries?count=20`
