# Elixir Plug Benchmarking Test

### Test Type Implementation Source Code

* [JSON](lib/framework_benchmarks/handlers/json.ex)
* [PLAINTEXT](lib/framework_benchmarks/handlers/plain-text.ex)
* [DB](lib/framework_benchmarks/handlers/db.ex)
* [QUERY](lib/framework_benchmarks/handlers/query.ex)
* [CACHED QUERY](lib/framework_benchmarks/handlers/cached-world.ex)
* [UPDATE](lib/framework_benchmarks/handlers/update.ex)
* [FORTUNES](lib/framework_benchmarks/handlers/fortune.ex)

## Important Libraries
The tests were run with:
* [Plug Cowboy - Webserver](https://github.com/elixir-plug/plug_cowboy)
* [ElJiffy - JSON encoder](https://github.com/lilrooness/eljiffy)
* [Ecto SQL - Fetching and Updating DB](https://github.com/elixir-ecto/ecto_sql)
* [Postgrex - Postgres SQL Adapter](https://github.com/elixir-ecto/postgrex)
* [Cachex - Cache](https://github.com/whitfin/cachex)
* [Phoenix HTML - HTML encoding](https://github.com/phoenixframework/phoenix_html)
* [ucol - Unicode ICU based collation](https://github.com/barrel-db/erlang-ucol)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=

### CACHED QUERY

http://localhost:8080/cached-worlds?count=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes
