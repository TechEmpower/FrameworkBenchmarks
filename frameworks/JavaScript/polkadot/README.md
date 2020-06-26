# Polkadot Benchmarking Test

This is the [`polkadot`](https://github.com/lukeed/polkadot) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

Information about Polkadot can be found at https://github.com/lukeed/polkadot

## Database Drivers

There are individual handlers for each DB approach. The logic for each of them are found here:

* [MySQL](drivers/mysql.js)
* [MongoDB](drivers/mongodb.js)
* [PostgreSQL](drivers/postgres.js)

There are **no database endpoints** or drivers attached by default.<br>
To initialize the application with one of these, run any _one_ of the following commands:

```sh
$ DATABASE=mysql node app.js
$ DATABASE=mongodb node app.js
$ DATABASE=postgres node app.js
```

## Test Endpoints

> Visit the test requirements [here](https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview)

```sh
$ curl localhost:8080/json
$ curl localhost:8080/plaintext

# The following are only available w/ DATABASE
# ---

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
