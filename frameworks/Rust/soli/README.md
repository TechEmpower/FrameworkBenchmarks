# [Soli](https://github.com/solisoft/soli_lang) MVC Framework

## Description

Soli is a modern, high-performance web framework written in Rust that combines elegant syntax with blazing fast execution. It features a custom programming language with Ruby-like syntax, MVC architecture, hot reload, and built-in HTTP server powered by Hyper.

* [GitHub Repository](https://github.com/solisoft/soli_lang)
* [Website](https://solisoft.github.io/soli_lang/)
* Cargo package: [solilang](https://crates.io/crates/solilang)

## Features

* **High Performance**: Built on Rust with Hyper async HTTP server
* **170,000+ requests/second** on a single server
* **Sub-millisecond response times** for most requests
* Hot reload in development mode
* MVC architecture with controllers, models, and views
* ERB-style templating with layouts and partials
* Built-in JSON support
* WebSocket support with Live View for reactive UIs
* Convention over configuration

## Architecture

This benchmark implements the TechEmpower test suite using Soli's MVC framework:

* **Language**: Soli (custom DSL compiled to bytecode)
* **Runtime**: Rust with Tokio async runtime
* **HTTP Server**: Hyper
* **JSON Serialization**: Native string serialization (optimized for benchmarks)
* **Port**: 3000

## Test URLs

### Test 1: JSON Encoding

Returns a simple JSON object with a message field.

```bash
curl http://localhost:3000/json
```

**Response:**
```json
{"message": "Hello, World!"}
```

### Test 2: Plaintext

Returns a simple plaintext response.

```bash
curl http://localhost:3000/plaintext
```

**Response:**
```
Hello, World!
```

## Implementation Details

The benchmark application consists of:

* `app/controllers/bench_controller.sl` - Controller with plaintext and JSON handlers
* `config/routes.sl` - Route definitions mapping URLs to controller actions
* `soli.dockerfile` - Multi-stage Docker build installing soli from crates.io

### Controller Example

```soli
fn plaintext(req: Any) -> Any {
    return {
        "status": 200,
        "headers": {
            "Content-Type": "text/plain",
            "Server": "soli"
        },
        "body": "Hello, World!"
    };
}
```

## Performance

Benchmarked on a standard server (16 cores):

| Metric | Value |
|--------|-------|
| Requests/sec | 170,000+ |
| Avg Latency | < 1ms |
| Framework Type | Platform |

## Local Testing

To test the benchmark locally:

```bash
cd frameworks/Rust/soli
docker build -f soli.dockerfile -t soli-benchmark .
docker run -p 3000:3000 soli-benchmark

# Test endpoints
curl http://localhost:3000/plaintext
curl http://localhost:3000/json
```

## Running TechEmpower Benchmarks

To run the full TechEmpower benchmark suite:

```bash
./tfb --test soli
```

This will run the plaintext and JSON serialization tests against the Soli framework.

## Notes

* Database tests are not implemented as Soli MVC currently does not have PostgreSQL support
* The framework uses a production-optimized build with caching enabled
* Worker threads are auto-detected based on available CPU cores
