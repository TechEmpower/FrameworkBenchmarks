# gnosis-uring

Topology-driven HTTP server with four primitives mapped directly to io_uring:

| Primitive | io_uring | Purpose |
|-----------|----------|---------|
| FORK | batch SQE submission | Accept connections, resolve files, race codecs |
| RACE | first CQE wins + cancel | Cache vs mmap vs disk, smallest codec wins |
| FOLD | gather CQEs | Headers + body, multi-frame responses |
| VENT | close fd, cancel ops | Timeout, error, connection close |

Features:
- Per-chunk Laminar codec racing (identity/gzip/brotli/deflate, smallest wins)
- SQPOLL mode (zero syscalls in hot path)
- Pinned buffers for stable io_uring SQE pointers
- UDP Aeon Flow transport (10-byte frames, no TCP overhead)

Whitepaper: https://forkracefold.com/

## Test URLs

- Plaintext: `http://localhost:8080/plaintext`
- JSON: `http://localhost:8080/json`
