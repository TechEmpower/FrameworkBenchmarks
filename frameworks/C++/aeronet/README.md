# Aeronet (TechEmpower Framework Benchmarks)

[aeronet](https://github.com/sjanel/aeronet) is a modern C++23 HTTP/1.1 + HTTP/2 server library focused on low overhead and high throughput. This TechEmpower implementation targets the JSON and Plaintext tests using glaze for fast JSON serialization.

## Test Endpoints

- `GET /json` -> `{"message":"Hello, World!"}`
- `GET /plaintext` -> `Hello, World!`

## Notes

- Built with C++23 and glaze 7.0.2.
- Linux-only (epoll-based).
- HTTP/1.1 keep-alive and pipelining supported.
- TLS, WebSocket, and HTTP/2 supported but not enabled in this benchmark.
- Other endpoints (e.g. database access) not implemented in this benchmark.
