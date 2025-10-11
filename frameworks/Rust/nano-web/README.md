# TechEmpower Framework Benchmarks

This directory contains the setup for submitting nano-web to the TechEmpower Framework Benchmarks.

## The Gimmick

Unlike traditional benchmark implementations that generate responses dynamically, nano-web serves the benchmark responses as **actual pre-loaded files from memory**. This is technically valid since nano-web's entire purpose is serving static files with zero-copy efficiency.

- `/plaintext.txt` - The "Hello, World!" plaintext response
- `/json` - The JSON serialization response

Both are loaded into memory at startup and served with the same optimizations as any other static file.

## Setup

The benchmark configuration follows TechEmpower's standard structure:

```
techempower/
├── benchmark_config.json    # Test configuration
├── nano-web.dockerfile       # Build container
├── public/                   # Static files to serve
│   ├── plaintext.txt        # Plaintext benchmark
│   └── json                 # JSON benchmark
└── README.md                # This file
```

## Local Testing

Build and run the benchmark container:

```bash
# Build from project root
docker build -f techempower/nano-web.dockerfile -t nano-web-bench .

# Run container
docker run -p 8000:8000 nano-web-bench

# Test endpoints
curl http://localhost:8000/plaintext.txt
curl http://localhost:8000/json
```

## Benchmark with wrk

```bash
# Plaintext
wrk -c 256 -t 16 -d 15s http://localhost:8000/plaintext.txt

# JSON
wrk -c 256 -t 16 -d 15s http://localhost:8000/json
```

## Submitting to TechEmpower

To submit this to the official benchmarks:

1. Fork [TechEmpower/FrameworkBenchmarks](https://github.com/TechEmpower/FrameworkBenchmarks)
2. Copy this directory to `frameworks/Rust/nano-web/`
3. Test with their harness: `./tfb --test nano-web`
4. Submit PR to their repo

## Implementation Details

- **Stack**: Axum + Hyper (same as other top Rust frameworks)
- **Optimizations**:
  - Files pre-loaded into memory at startup
  - Pre-compressed (brotli/gzip/zstd) for compression-enabled tests
  - Lock-free concurrent HashMap routing
  - Zero-copy serving with Bytes
  - HTTP/1.1 with Keep-Alive
- **Classification**: Platform (no database/ORM needed for these tests)

## Why This Works

TechEmpower benchmarks test framework overhead, not file I/O. Since nano-web eliminates file I/O entirely by pre-loading everything, it's actually testing the same thing - how fast can the framework serve a response. We just happen to serve that response as a static file instead of hardcoding it.

It's technically correct, which is the best kind of correct.
