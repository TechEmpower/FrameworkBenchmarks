# touchsocket benchmarks

See [touchsocket](https://touchsocket.net/) for more information.

## Variants Included

* `touchsocket.webapi` (`TouchSocketWebApi`) – WebApi style.
* `touchsocket.webapi31` (`TouchSocketWebApi31`) – WebApi targeting .NET 8.
* `touchsocket.http` (`TouchSocketHttp`) – Minimal HTTP implementation.
* `touchsocket.http31` (`TouchSocketHttp31`) – Minimal HTTP targeting .NET 8.
* `touchsocket.httpplatform` (`TouchSocketHttpPlatform`) – High-performance custom pipeline-based HTTP server focusing on low-level parsing and zero-allocation response writing.

## Infrastructure Software Versions

**Language / Runtime**

* C# / .NET (8.0 & 9.0 depending on variant)

## Endpoints

All variants implement:

* `/plaintext` – Returns a plain text "Hello, World!" response.
* `/json` – Returns a JSON object `{"message": "Hello, World!"}`.

The `httpplatform` variant manually parses request lines and headers via `System.IO.Pipelines` for maximum throughput.

## Dockerfiles

Each variant has a dedicated Dockerfile named:

* `touchsocket.dockerfile` (webapi)
* `touchsocket-webapi31.dockerfile`
* `touchsocket-http.dockerfile`
* `touchsocket-http31.dockerfile`
* `touchsocket-httpplatform.dockerfile`

## Notes

The `httpplatform` variant is intended for benchmarking raw server performance. It omits higher-level abstractions to reduce overhead.

