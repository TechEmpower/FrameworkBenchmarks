# [hyperlane](https://github.com/hyperlane-dev/hyperlane) web framework

## Description

> A lightweight, high-performance, and cross-platform rust http server library built on tokio, it simplifies modern web service development by providing built-in support for middleware, websocket, server-sent events (sse), and raw tcp communication, while offering a unified and ergonomic api across windows, linux, and macos, enabling developers to build robust, scalable, and event-driven network applications with minimal overhead and maximum flexibility.

## Database

PostgreSQL.

## Test URLs

### Test 1: JSON Encoding

    http://localhost:8080/json

### Test 2: Single Row Query

    http://localhost:8080/db

### Test 3: Multi Row Query

    http://localhost:8080/query?q=20

### Test 4: Fortunes (Template rendering)

    http://localhost:8080/fortunes

### Test 5: Update Query

    http://localhost:8080/upda?q=20

### Test 6: Plaintext

    http://localhost:8080/plaintext

### Test 7: Caching

    http://localhost:8080/cached-quer?c=20
