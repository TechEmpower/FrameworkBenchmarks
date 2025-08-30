# [Ohkami](https://github.com/ohkami-rs/ohkami) - A performant, declarative, and runtime-flexible web framework for Rust

## Features

> - *macro-less and type-safe* APIs for declarative, ergonomic code
> - *runtime-flexible* ： `tokio`, `smol`, `nio`, `glommio` and `worker` (Cloudflare Workers), `lambda` (AWS Lambda)
> - good performance, no-network testing, well-structured middlewares, Server-Sent Events, WebSocket, highly integrated OpenAPI document generation, ...

## Test URLs

### 1. JSON Serialization

    http://localhost:8000/json

### 2. Single Database Query

    http://localhost:8000/db

### 3. Multiple Database Queries

    http://localhost:8000/queries?q={count}

### 4. Fortunes

    http://localhost:8000/fortunes

### 5. Database Updates

    http://localhost:8000/updates?q={count}

### 6. Plaintext

    http://localhost:8000/plaintext
