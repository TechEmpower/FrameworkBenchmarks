<div align="center">
  <h1>Roa</h1>
  <p><strong>Roa is an async web framework inspired by koajs, lightweight but powerful. </strong> </p>
  <p>

[![Stable Test](https://github.com/Hexilee/roa/workflows/Stable%20Test/badge.svg)](https://github.com/Hexilee/roa/actions)
[![codecov](https://codecov.io/gh/Hexilee/roa/branch/master/graph/badge.svg)](https://codecov.io/gh/Hexilee/roa) 
[![wiki](https://img.shields.io/badge/roa-wiki-purple.svg)](https://github.com/Hexilee/roa/wiki)
[![Rust Docs](https://docs.rs/roa/badge.svg)](https://docs.rs/roa)
[![Crate version](https://img.shields.io/crates/v/roa.svg)](https://crates.io/crates/roa)
[![Download](https://img.shields.io/crates/d/roa.svg)](https://crates.io/crates/roa)
[![Version](https://img.shields.io/badge/rustc-1.40+-lightgray.svg)](https://blog.rust-lang.org/2019/12/19/Rust-1.40.0.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/Hexilee/roa/blob/master/LICENSE)

  </p>

  <h3>
    <a href="https://github.com/Hexilee/roa/tree/master/examples">Examples</a>
    <span> | </span>
    <a href="https://github.com/Hexilee/roa/wiki/Guide">Guide</a>
    <span> | </span>
    <a href="https://github.com/Hexilee/roa/wiki/Cookbook">Cookbook</a>
  </h3>
</div>
<br>


## Feature highlights

- A lightweight, solid and well extensible core.
    - Supports HTTP/1.x and HTTP/2.0 protocols.
    - Full streaming.
    - Highly extensible middleware system.
    - Based on [`hyper`](https://github.com/hyperium/hyper), runtime-independent, you can chose async runtime as you like.
- Many useful extensions and middlewares.
    - Transparent content compression (br, gzip, deflate, zstd).
    - Configurable and nestable router.
    - Named uri parameters(query and router parameter).
    - Cookie and jwt support.
    - Integration with [serde](https://github.com/serde-rs/serde) and [askama](https://github.com/djc/askama). JSON, urlencoded form, html template support.
    - ORM integration (with [diesel](https://github.com/diesel-rs/diesel)).
    - HTTPS support.
    - WebSocket support.
    - GraphQL support(based on [juniper](https://github.com/graphql-rust/juniper)).
    - Asynchronous multipart form support.
    - Other middlewares(logger, CORS .etc).
- Works on stable Rust.

## Database

PostgreSQL.

* ORM using [diesel](http://diesel.rs)

## Test URLs

### Test 1: JSON Encoding

    http://localhost:8080/json

### Test 2: Single Row Query

    http://localhost:8080/db

### Test 3: Multi Row Query

    http://localhost:8080/queries?q=20

### Test 4: Fortunes (Template rendering)

    http://localhost:8080/fortune

### Test 5: Update Query

    http://localhost:8080/updates?q=20

### Test 6: Plaintext

    http://localhost:8080/plaintext

