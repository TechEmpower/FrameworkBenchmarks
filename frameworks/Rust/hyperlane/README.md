# [hyperlane](https://github.com/hyperlane-dev/hyperlane) web framework

## Description

> A lightweight rust http server with middleware, websocket, sse, and tcp support, built on tokio for cross-platform async networking, hyperlane simplifies modern web service development.

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
