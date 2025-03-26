# [hyperlane](https://github.com/ltpp-universe/hyperlane) web framework

## Description

Hyperlane is a lightweight and high-performance Rust HTTP server library designed to simplify network service development. It supports HTTP request parsing, response building, and TCP communication, making it ideal for building modern web services. Additionally, it provides support for request and response middleware, WebSocket, and Server-Sent Events (SSE), enabling flexible and efficient real-time communication.

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
