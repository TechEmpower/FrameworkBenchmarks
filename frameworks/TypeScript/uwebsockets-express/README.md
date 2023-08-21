# uWebSockets-express Benchmarking Test

[uWebSockets-express](https://github.com/colyseus/uWebSockets-express) provides Express API compatibility layer for uWebSockets.js. For compability coverage and middleware supports please refer to its documentation.

[µWebSockets.js](https://github.com/uNetworking/uWebSockets.js) is a web server bypass for Node.js.

### Disclaimer

Having an Express compatibility layer on top of µWebSockets.js is going to bring its performance slightly down. This library does not have performance as its only objective.

Although uWebSockets-express has uWebsockets in its name, it is not a part of uWebSockets.js project. It is a separate project maintained by [Colyseus](https://github.com/colyseus) authors.

## Important Libraries

The tests were run with:

- [uWebSockets-express](https://github.com/colyseus/uWebSockets-express) 
- [express](https://github.com/expressjs/express) 
- [uWebSockets.js](https://github.com/uNetworking/uWebSockets.js)
- [postgres](https://github.com/porsager/postgres)
- [drizzle-orm](https://github.com/drizzle-team/drizzle-orm)
- [ejs](https://github.com/mde/ejs)

## Test Endpoints

> Visit the test requirements [here](https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview)

```sh
$ curl localhost:8080/json

$ curl localhost:8080/plaintext

$ curl localhost:8080/db

$ curl localhost:8080/fortunes

$ curl localhost:8080/queries?n=2
$ curl localhost:8080/queries?n=0
$ curl localhost:8080/queries?n=foo
$ curl localhost:8080/queries?n=501
$ curl localhost:8080/queries?n=

$ curl localhost:8080/updates?n=2
$ curl localhost:8080/updates?n=0
$ curl localhost:8080/updates?n=foo
$ curl localhost:8080/updates?n=501
$ curl localhost:8080/updates?n=
```
