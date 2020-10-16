# Morph Benchmarking Test

### Test Type Implementation Source Code

* [JSON](./src/bin/Json_handler.re)
* [PLAINTEXT](./src/bin/Json_handler.re)
* [DB](./src/bin/Db_handler.re)
* [QUERY](./src/bin/Query_handler.re)
<!--
* [CACHED QUERY](Relative/Path/To/Your/Source/File)
* [UPDATE](Relative/Path/To/Your/Source/File)
* [FORTUNES](Relative/Path/To/Your/Source/File)
-->

## Important Libraries
The tests were run with:
* [Morph](https://github.com/reason-native-web/morph)
* [<ttpaf (fork)](https://github.com/anmonteiro/httpaf)
* [Caqti](https://github.com/paurkedal/ocaml-caqti)
* [Lwt](https://github.com/ocsigen/lwt)
* [Yojson](https://github.com/ocaml-community/yojson)
* [Ptime](https://github.com/dbuenzli/ptime)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=

<!--
### CACHED QUERY

http://localhost:8080/cached_query?queries=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes
-->
