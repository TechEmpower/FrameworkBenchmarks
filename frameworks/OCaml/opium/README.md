# Opium Benchmarking Test

### Test Type Implementation Source Code

* [PLAINTEXT](src/lib/routes.ml#L3-5)
* [JSON](src/lib/routes.ml#L7-10)
* [DB](src/lib/routes.ml#L12-L20)
* [QUERY](src/lib/routes.ml#L37-L41)
* [UPDATES](src/lib/routes.ml#L43-L56)
* [FORTUNES](src/lib/routes.ml#L58-L67)

## Important Libraries
* [Opium](https://github.com/rgrinberg/opium)
* [Httpaf](https://github.com/inhabitedtype/httpaf)
* [Caqti](https://github.com/paurkedal/ocaml-caqti)
* [Lwt](https://github.com/ocsigen/lwt)
* [Yojson](https://github.com/ocaml-community/yojson)
* [Ppx_rapper](https://github.com/roddyyaga/ppx_rapper)

## Test URLs

### PLAINTEXT

http://localhost:8080/plaintext

### JSON

http://localhost:8080/json

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/queries/

### UPDATES

http://localhost:8080/updates/

### FORTUNES

http://localhost:8080/fortunes/
