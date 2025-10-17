# TFB MySQLHaskell

`mysql-haskell` backend for TFB benchmarks that can re-used with any server.

Note: Currently broken, as test server uses `caching_sha2_password` authentication,
but library mysql-haskell does not support this yet.
