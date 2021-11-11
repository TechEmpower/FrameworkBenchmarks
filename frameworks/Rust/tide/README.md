# tide

[Tide](https://github.com/http-rs/tide) is a minimal and pragmatic Rust web application framework built for rapid development. It comes with a robust set of features that make building async web applications and APIs easier and more fun.

## Benchmark Implementation

This implementation is largely based on the `actix` and `warp` implementation. Thanks to everyone who worked on them.

### Database

PostgreSQL, ORM using [diesel](http://diesel.rs), the de facto standard for Rust ORM.

### Routes

| Benchmark | Name                          | URL                                  |
| --------- | ----------------------------- | ------------------------------------ |
| Test 1    | JSON Encoding                 | http://localhost:8080/json           |
| Test 2    | Single Row Query              | http://localhost:8080/db             |
| Test 3    | Multi Row Query               | http://localhost:8080/queries/:count |
| Test 4    | Fortunes (Template rendering) | http://localhost:8080/fortune        |
| Test 5    | Update Query                  | http://localhost:8080/updates/:count |
| Test 6    | Plaintext                     | http://localhost:8080/plaintext      |

### Implementation

Routing is defined in [`main.rs`](src/main.rs).
Top level request handling is defined in [`handlers.rs`](src/handlers.rs).
