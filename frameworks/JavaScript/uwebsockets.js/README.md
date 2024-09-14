# uWebSockets.js Benchmarking Test

uWebSockets is a web server written in C/C++ (https://github.com/uNetworking/uWebSockets)

µWebSockets.js is a web server bypass for Node.js (https://github.com/uNetworking/uWebSockets.js)

## Important Libraries

The tests were run with:

- [uWebSockets.js](https://github.com/uNetworking/uWebSockets.js/)
- [postgres](https://github.com/porsager/postgres/)

## Database

There are individual handlers for each DB approach. The logic for each of them are found here:

- [PostgreSQL](src/database/postgres.js)

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
