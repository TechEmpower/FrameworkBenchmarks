# Hono Benchmarking Test

Hono - [炎] means flame🔥 in Japanese - is a small, simple, and ultrafast web framework for the Edges. It works on any JavaScript runtime: Cloudflare Workers, Fastly Compute, Deno, Bun, Vercel, Lagon, AWS Lambda, Lambda@Edge, and Node.js. https://github.com/honojs/hono

## Important Libraries

The tests were run with:

- [Hono](https://github.com/honojs/hono)
- [Postgres.js](https://github.com/porsager/postgres/)

## Database

There are individual handlers for each DB approach. The logic for each of them are found here:

- [PostgreSQL](database/postgres.js)

There are **no database endpoints** or drivers attached by default.

To initialize the application with one of these, run any _one_ of the following commands:

```sh
$ DATABASE=postgres npm start
```

## Test Endpoints

> Visit the test requirements [here](https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview)

```sh
$ curl localhost:8080/json
$ curl localhost:8080/plaintext

# The following are only available with the DATABASE env var

$ curl localhost:8080/db
$ curl localhost:8080/fortunes

$ curl localhost:8080/updates?queries=
$ curl localhost:8080/updates?queries=2
$ curl localhost:8080/updates?queries=1000
$ curl localhost:8080/updates?queries=foo
$ curl localhost:8080/updates?queries=0

$ curl localhost:8080/queries?queries=
$ curl localhost:8080/queries?queries=2
$ curl localhost:8080/queries?queries=1000
$ curl localhost:8080/queries?queries=foo
$ curl localhost:8080/queries?queries=0
```
