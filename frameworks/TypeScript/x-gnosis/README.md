# x-gnosis

[x-gnosis](https://github.com/affectively-ai/x-gnosis) is an nginx-config-compatible web server built on Aeon Flow topology scheduling. Every layer of request processing uses fork/race/fold primitives:

- **File resolution**: `race(cache, mmap, disk)` -- first to complete wins
- **Compression**: Per-chunk codec racing (identity/gzip/brotli/deflate, smallest wins)
- **Transport**: 10-byte Aeon Flow frames (3x less overhead than HTTP/3)

For these TechEmpower benchmarks, x-gnosis uses Bun.serve with multi-process spawning, matching the Bun baseline entry pattern.

## Test URLs

- Plaintext: `http://localhost:8080/plaintext`
- JSON: `http://localhost:8080/json`
