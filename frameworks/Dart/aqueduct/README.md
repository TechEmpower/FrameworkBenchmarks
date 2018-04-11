# Aqueduct Framework Benchmarking Test

This test adds [Aqueduct](https://aqueduct.io), a microframework for Dart, to the [benchmarking test suite](../). The test is based on the Dart Benchmarking Test.

## Versions

* [Dart SDK version >=1.7.0](http://www.dartlang.org/)
* [Dart aqueduct version 2.5.0+1](https://pub.dartlang.org/packages/aqueduct)

## Test URLs

### Common

#### JSON Encoding Test
http://localhost:8080/json

#### Plaintext Test
http://localhost:8080/plaintext


### PostgreSQL

#### Data-Store/Database Mapping Test
http://localhost:8080/pg/db

#### Variable Query Test
http://localhost:8080/pg/queries?queries=2

#### Fortunes Test
http://localhost:8080/pg/fortunes

#### Data-Store/Database Update Test
http://localhost:8080/pg/updates

#### Variable Update Test
http://localhost:8080/pg/updates?queries=2


### MongoDB

#### Data-Store/Database Mapping Test
http://localhost:8080/mongo/db

#### Variable Query Test
http://localhost:8080/mongo/queries?queries=2

#### Fortunes Test
http://localhost:8080/mongo/fortunes

#### Data-Store/Database Update Test
http://localhost:8080/mongo/updates

#### Variable Update Test
http://localhost:8080/mongo/updates?queries=2


