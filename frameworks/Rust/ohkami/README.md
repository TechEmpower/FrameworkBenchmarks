# [ohkami](https://github.com/kana-rus/ohkami) - Intuitive and Declarative Web Framework for Rust

## Description

> Build web app in intuitive and declarative code
> - *macro-less and type-safe* APIs for intuitive and declarative code
> - *multi runtime* support：`tokio`, `async-std`, `worker` (Cloudflare Workers)

- [User Guide](https://docs.rs/ohkami/latest/ohkami/)
- [API Documentation](https://docs.rs/ohkami/latest/ohkami/)
- Cargo package: [ohkami](https://crates.io/crates/ohkami)

## Database

PostgreSQL with [sqlx](https://github.com/launchbadge/sqlx)

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