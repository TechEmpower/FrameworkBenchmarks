# Dream

## Overview

Most of all of the code is inside of `test_dream/bin/main.ml` file. 

## Implemented tests

| Test Name  | Endpoint                      |
|------------|-------------------------------|
| Plain text | http://0.0.0.0:8080/plaintext |
| Json       | http://0.0.0.0:8080/json      |

## Headers

A simple middleware was added that adds the required headers for the Techempower Benchmarks.
The date header is refreshed only once per second as allowed by the rules for performance.

## Dependencies

The `test_dream/dune-project` and `test_dream/bin/dune` are where dependencies are managed 
for this project. If you add a dependency to those locations and then run the following:

```
cd test_dream
dune build test_dream.opam
opam install --yes --deps-only .
```

You will update the opam package list and install your changes locally.

## Running tests

$ tfb --mode verify --test dream

