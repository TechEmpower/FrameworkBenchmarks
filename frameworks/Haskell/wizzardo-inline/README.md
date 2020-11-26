# wizzardo-inline Benchmarking Test

### Test Type Implementation Source Code

* [JSON](wizzardo-http-benchmark/src/main/haskell/Main.hs)
* [PLAINTEXT](wizzardo-http-benchmark/src/main/haskell/Main.hs)
* [DB](wizzardo-http-benchmark/src/main/haskell/DbHandler.hs)

## Important Libraries

These benchmarks measure the [wizzardo-http][wizzardo-http] server when given
Haskell callbacks using [inline-java][inline-java].

Furtheremore, an experimental interface of inline-java is used, where
[-XLinearTypes][linear-types] ensures references to Java objects are
handled correctly on the Haskell side.

[linear-types]: https://github.com/tweag/ghc-proposals/blob/linear-types2/proposals/0000-linear-types.rst
[inline-java]: https://github.com/tweag/inline-java
[wizzardo-http]: https://github.com/wizzardo/webery

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db
