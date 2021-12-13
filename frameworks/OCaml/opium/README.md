# Opium Benchmarking Test

### Test Type Implementation Source Code

* [PLAINTEXT](src/lib/routes.ml#L3-5)
* [JSON](src/lib/routes.ml#L7-10)
* [DB](src/lib/routes.ml#L12-L20)
* [QUERY](src/lib/routes.ml#L37-L41)
* [UPDATES](src/lib/routes.ml#L43-L56)
* [FORTUNES](src/lib/routes.ml#L58-L67)

## Important Libraries
* [Lwt](https://github.com/ocsigen/lwt)
* [Opium](https://github.com/rgrinberg/opium)
* [Httpaf](https://github.com/inhabitedtype/httpaf)
* [Caqti](https://github.com/paurkedal/ocaml-caqti)
* [Ppx_rapper](https://github.com/roddyyaga/ppx_rapper)
* [Yojson](https://github.com/ocaml-community/yojson)
* [Ppx_deriving_yojson](https://github.com/ocaml-ppx/ppx_deriving_yojson)
* [Tyxml](https://github.com/ocsigen/tyxml)

## Structure
`lib/` contains most of the logc.
It doesn't have a dependency on any web server to make it more portable and allow easier addition to new webservers.
Feel free to copy paste when adding additional servers.

## Local development

Either use the docker images or `make pin && make install` and then `make run` or `make run-forks` 

## Variants

- opium - base implementation, uses a single process
- opium-haproxy - starts multiple processes on different ports and uses haproxy to distribute the load
- opium-fedora-forks - starts multiple processes listening on the same socket
- opium-alpine-forks - same as the above, but uses `alpine` instead of `fedora`

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
