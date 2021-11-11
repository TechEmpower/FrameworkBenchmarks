# Servant Shared

This is a generic test that produces an executable for each supported backend library:

- `servant-hasql`: PostgreSQL database via the [`hasql`](https://github.com/nikita-volkov/hasql) library.
- `servant-mysql-haskell`: MySQL database via the [`mysql-haskell`](https://github.com/winterland1989/mysql-haskell) library.
- `servant-postgres-wire` (default): PostgreSQL database via the [`postgres-wire`](https://github.com/postgres-haskell/postgres-wire) library.

**NOTE**: the `shared` directory here is a full copy of the same directory within the `warp` framework dir. TODO: wait for TFB to [add support for language scope dockerfile](https://github.com/TechEmpower/FrameworkBenchmarks/pull/4595#issuecomment-478593547) before moving `shared` up into the Haskell language dir so both `servant` and `warp` can re-use the same modules.
