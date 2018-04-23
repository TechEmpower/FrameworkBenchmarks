# Aqueduct Framework Benchmarking Test

This test adds [Aqueduct](https://aqueduct.io), a microframework for Dart, to the [benchmarking test suite](../). The test is based on the Dart Benchmarking Test.

## Versions

* [Dart SDK version >=1.7.0](http://www.dartlang.org/)
* [Dart aqueduct version 2.5.0+1](https://pub.dartlang.org/packages/aqueduct)
* [Dart Mustache version 1.0.0](https://pub.dartlang.org/packages/mustache)

## Test URLs

### Common

#### JSON Encoding Test
http://localhost:8080/json

#### Plaintext Test
http://localhost:8080/plaintext


### PostgreSQL

#### Data-Store/Database Mapping Test
http://localhost:8080/db

#### Variable Query Test
http://localhost:8080/queries/2

#### Fortunes Test
http://localhost:8080/fortunes

#### Data-Store/Database Update Test
http://localhost:8080/updates

#### Variable Update Test
http://localhost:8080/updates/2
