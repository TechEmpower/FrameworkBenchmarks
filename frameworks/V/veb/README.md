# V lang example web server

Server "competition" *Web Framework Benchmarks*: https://www.techempower.com/benchmarks

It designed to demonstrate how to use <a href="https://modules.vlang.io/veb.html">Veb</a>'s built-in features to create a web application. It's a slightly more complex example due to the fact i tried to reuse code for multiple scenarios.

> NOTE: Cache test still to be properly implemented

## Scenarios

We created some scenarios. One on each folder. All scenarios use **Veb** server, the <a href="https://vlang.io">V</a> lang built-in web framework.

Scenario | Run locally | Run inside Benchmark
 --- | --- | ---
Veb with Postgres | `db_host="127.0.0.1" v run veb-pg/` | `./tfb --mode veb`
Veb with Postgres and ORM | `db_host="127.0.0.1" v run veb-pg-orm/` | `./tfb --mode veb-pg-orm`
Veb with MySQL/MariaDB | `db_host="127.0.0.1" v run veb-my/` | `./tfb --mode veb-my`
Veb with MySQL/MariaDB and ORM | `db_host="127.0.0.1" v run veb-my-orm/` | `./tfb --mode veb-my-orm`
Veb with Postgres (Optimized) | `db_host="127.0.0.1" v run veb-pg/` | `./tfb --mode veb-pg-optimized`
Veb with Postgres (Compilation flags) | `db_host="127.0.0.1" v run veb-pg/` | `./tfb --mode veb-compilation`

## Local Setup

in order to test the servers locally, you will need a database server and a database client. using container, you can use:

### Postgres:

```
docker run --name benckmark-postgres \
  -e POSTGRES_USER=benchmarkdbuser \
  -e POSTGRES_PASSWORD=benchmarkdbpass \
  -e POSTGRES_DB=hello_world \
  -p 5432:5432 \
  -d techempower/postgres:latest
```

### MySQL/MariaDB:

```
docker run --name benckmark-mysql \
  -e MYSQL_ROOT_PASSWORD=benchmarkdbpass \
  -e MYSQL_USER=benchmarkdbuser \
  -e MYSQL_PASSWORD=benchmarkdbpass \
  -e MYSQL_DATABASE=hello_world \
  -p 3306:3306 \
  -d techempower/mysql:latest
```

---

Created by Bruno Massa
