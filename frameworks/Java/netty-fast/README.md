# Netty (Fast / Minimal)

This is a Netty-based implementation for the TechEmpower Framework Benchmarks.
It is a minimal, high-performance HTTP/1.1 server built directly on Netty
primitives with a focus on correctness and low overhead.

The implementation uses real HTTP parsing, proper response headers, and
standards-compliant behavior while aggressively minimizing allocations and
framework abstractions.

## Tests

### Plaintext Test
Responds with a static plaintext message.

GET /plaintext

Response body:
Hello, World!

Content-Type:
text/plain

### JSON Test
Responds with a JSON-encoded object using a real JSON serializer.

GET /json

Response body:
{"message":"Hello, World!"}

Content-Type:
application/json

JSON serialization is performed using fastjson2.

## Implementation Notes

- HTTP/1.1 only
- Pipelining supported
- Uses Netty event loops and pooled buffers
- Minimal channel pipeline
- No framework abstractions beyond Netty itself
- Optimized for low allocation rate and high throughput
- Designed to match TechEmpower benchmark rules and expectations

This implementation is intended to demonstrate the maximum achievable
performance of Netty when used as a low-level HTTP server.

## Versions

- Java 24+ (tested with Java 24 / 25)
- Netty 4.2.x
- fastjson2

## References

- https://netty.io/
- https://github.com/netty/netty
- https://github.com/TechEmpower/FrameworkBenchmarks
