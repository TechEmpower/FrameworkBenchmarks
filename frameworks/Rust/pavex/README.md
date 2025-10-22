# pavex Benchmarking Test

Benchmark implementations for [pavex](https://pavex.dev).

## How to update

The `pavex_cli` binary must be installed in order to (re)generate the `api_sdk` library crate.  
You can install it via `cargo` using:

```bash
# Replace `XXXXXX` with the revision of `pavex` found in `blueprint/Cargo.toml`.
cargo install pavex_cli --git "https://github.com/LukeMathWalker/pavex.git" --rev XXXXXX
```

You should also install the `cargo-px` CLI:

```bash
cargo install cargo-px@0.1.4 --locked 
```

You can then (re)generate `api_sdk` by running:

```bash
cargo px check
```

## Test URLs
### JSON

Not implemented

### PLAINTEXT

http://localhost:8000/plaintext

### DB

Not implemented

### QUERY

Not implemented

### CACHED QUERY

Not implemented

### UPDATE

Not implemented

### FORTUNES

Not implemented
