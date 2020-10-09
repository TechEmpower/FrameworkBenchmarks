# Opium Benchmarking Test

### Test Type Implementation Source Code

* [JSON](src/lib/routes.ml#L7-10)
* [PLAINTEXT](src/lib/routes.ml#L3-5)
* [DB](src/lib/routes.ml#L12-L21)
* [QUERY](src/lib/routes.ml#L23-L40)

## Important Libraries
* [Opium](https://github.com/rgrinberg/opium)
* [Httpaf](https://github.com/inhabitedtype/httpaf)
* [Caqti](https://github.com/paurkedal/ocaml-caqti)
* [Lwt](https://github.com/ocsigen/lwt)
* [Yojson](https://github.com/ocaml-community/yojson)
* [Ppx_rapper](https://github.com/roddyyaga/ppx_rapper)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/queries/
