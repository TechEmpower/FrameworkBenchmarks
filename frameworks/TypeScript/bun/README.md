# [Bun](https://github.com/oven-sh/bun) Benchmarking Test

[Bun](https://github.com/oven-sh/bun) is an all-in-one toolkit for JavaScript and TypeScript apps. It ships as a single executable called `bun​`.

At its core is the [Bun runtime](https://bun.sh/docs#what-is-a-runtime), a fast JavaScript runtime designed as a drop-in replacement for Node.js. It's written in Zig and powered by JavaScriptCore under the hood, dramatically reducing startup times and memory usage.

HTTP & HTTPS Server is powered by a fork of [uWebSockets](https://github.com/uNetworking/uWebSockets). [🔗](https://github.com/oven-sh/bun/blob/main/packages/bun-types/bun.d.ts#L2050)

## Important Libraries

The tests were run with:

* [bun](https://github.com/oven-sh/bun)
* [postgres](https://github.com/porsager/postgres)
* [fast-url-parser](https://github.com/petkaantonov/urlparser)

## Test Type Implementation Source Code

* [JSON](./src/app.ts#L9)
* [PLAINTEXT](./src/app.ts#L7)
* [DB](./src/app.ts#L12)
* [QUERY](./src/app.ts#L40)
* [UPDATE](./src/app.ts#L51)
* [FORTUNES](./src/app.ts#L17)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/queries?n=

### UPDATE

http://localhost:8080/updates?n=

### FORTUNES

http://localhost:8080/fortunes