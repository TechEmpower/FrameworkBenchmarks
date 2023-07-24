# [Bun](https://github.com/oven-sh/bun) Benchmarking Test

Bun is an all-in-one toolkit for JavaScript and TypeScript apps. It ships as a single executable called `bun​`.

At its core is the [Bun runtime](https://bun.sh/docs#what-is-a-runtime), a fast JavaScript runtime designed as a drop-in replacement for Node.js. It's written in Zig and powered by JavaScriptCore under the hood, dramatically reducing startup times and memory usage.

## Important Libraries
The tests were run with:
* [bun](https://github.com/oven-sh/bun)
* [postgres](https://github.com/porsager/postgres)
* [fast-url-parser](https://github.com/petkaantonov/urlparser)

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