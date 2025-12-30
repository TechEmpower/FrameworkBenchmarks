# HyperExpress Benchmarking Test

HyperExpress is a high performance Node.js webserver with a simple-to-use API powered by µWebSockets.js under the hood. (https://github.com/kartikk221/hyper-express)

µWebSockets.js is a web server bypass for Node.js (https://github.com/uNetworking/uWebSockets.js)

## Important Libraries

The tests were run with:

- [hyper-express](https://github.com/kartikk221/hyper-express)
- [postgres](https://github.com/porsager/postgres)
- [mariadb](https://github.com/mariadb-corporation/mariadb-connector-nodejs)
- [lru-cache](https://github.com/isaacs/node-lru-cache)

## Database

There are individual handlers for each DB approach. The logic for each of them are found here:

- [Postgres](database/postgres.js)
- [MySQL](database/mysql.js)

There are **no database endpoints** or drivers attached by default.

To initialize the application with one of these, run any _one_ of the following commands:

```sh
$ DATABASE=postgres npm start
$ DATABASE=mysql npm start
```

## Test Endpoints

> Visit the test requirements [here](https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview)

```sh
$ curl localhost:8080/json
$ curl localhost:8080/plaintext

# The following are only available with the DATABASE env var

$ curl localhost:8080/db
$ curl localhost:8080/fortunes

$ curl localhost:8080/queries?queries=2
$ curl localhost:8080/queries?queries=0
$ curl localhost:8080/queries?queries=foo
$ curl localhost:8080/queries?queries=501
$ curl localhost:8080/queries?queries=

$ curl localhost:8080/updates?queries=2
$ curl localhost:8080/updates?queries=0
$ curl localhost:8080/updates?queries=foo
$ curl localhost:8080/updates?queries=501
$ curl localhost:8080/updates?queries=

$ curl localhost:8080/cached-worlds?count=2
$ curl localhost:8080/cached-worlds?count=0
$ curl localhost:8080/cached-worlds?count=foo
$ curl localhost:8080/cached-worlds?count=501
$ curl localhost:8080/cached-worlds?count=
```
