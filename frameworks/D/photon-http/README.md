# photon-http benchmark

A benchmark for photon fiber scheduler for D running with minimalistic http server library photon-http.

# photon-http Benchmarking Test

### Test Type Implementation Source Code

* [JSON](./source/app.d)
* [PLAINTEXT](./source/app.d)
* [DB](./source/app.d)
* [QUERIES](./source/app.d)
* [UPDATES](./source/app.d)

## Important Libraries

The tests were run with:
* [photon](https://github.com/DmitryOlshansky/photon)
* [photon-http][https://github.com/DmitryOlshansky/photon-http]
* [dpq2](https://github.com/denizzzka/dpq2)

## Test URLs

### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### Data-Store/Database Mapping Test

http://localhost:8080/db

### Multiple queries

http://localhost:8080/queries?queries=...

### Multiple queries with updates

http://localhost:8080/updates?queries=...

