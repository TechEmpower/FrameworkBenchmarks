# Servant + mysql-haskell

This test uses MySQL via the [`mysql-haskell`](https://hackage.haskell.org/package/mysql-haskell) library.

Since both the server and the database clients are written in **pure** haskell, this implementation should easily beat `libpq`/`libmysql` dependent implementations without the overhead of foreign function calls.
