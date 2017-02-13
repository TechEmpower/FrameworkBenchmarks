# Pedestal Benchmarking Test

This is the [Pedestal](https://github.com/pedestal/pedestal) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Quick testing/benching

You can use the `run.sh` and `perfit.sh` scripts to get a rough idea how
changes will impact a final benchmark.

## Test URLs
### JSON Encoding Test
`http://localhost:8080/json`

### Single Query Test
`http://localhost:8080/db`

### Multiple Query Test
`http://localhost:8080/queries?queries=number`

### Fortune Test
`http://localhost:8080/fortunes`

### Database Updates
`http://localhost:8080/updates?queries=number`

### Plaintext
`http://localhost:8080/plaintext`

