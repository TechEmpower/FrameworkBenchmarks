# Nexios Framework Benchmark

This directory contains the Nexios framework implementation for the TechEmpower Web Framework Benchmarks.

## About Nexios

Nexios is a modern, high-performance ASGI web framework that combines high performance with developer-friendly features. Built on proven design patterns while introducing modern capabilities like dependency injection, automatic OpenAPI documentation, and comprehensive middleware support.

## Test Implementations

This benchmark includes multiple server configurations:

- **default**: Nexios with Uvicorn (standard configuration)
- **gunicorn**: Nexios with Gunicorn server
- **uvicorn**: Nexios with Uvicorn ASGI server
- **granian**: Nexios with Granian server (Rust-based ASGI server)
- **socketify-asgi**: Nexios with Socketify ASGI server (C++-based)

## Benchmark Tests

- **JSON**: Simple JSON serialization
- **DB**: Single database query
- **Queries**: Multiple database queries (1-500)
- **Fortunes**: HTML template rendering with database data
- **Updates**: Database updates with multiple queries
- **Plaintext**: Simple text response

## Running the Benchmarks

```bash
# Build all images
./run_benchmarks.sh

# Run individual containers
docker run -p 8080:8080 nexios-gunicorn
docker run -p 8080:8080 nexios-uvicorn
docker run -p 8080:8080 nexios-granian
docker run -p 8080:8080 nexios-socketify
```

## Configuration

- **Database**: PostgreSQL with connection pooling
- **Workers**: CPU count optimized
- **Port**: 8080 (standard for TechEmpower)
- **Host**: 0.0.0.0 (bind to all interfaces)
- **Logging**: Error level only for performance

## Framework Comparison

Nexios vs FastAPI:
- Similar ASGI foundation
- Built-in dependency injection
- Automatic OpenAPI documentation
- Comprehensive middleware system
- WebSocket support
- Pydantic integration

## License

This benchmark implementation is part of the TechEmpower Web Framework Benchmarks project. 