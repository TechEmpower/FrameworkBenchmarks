# Dart Start Framework Benchmarking Test

This test adds [Stream](https://github.com/rikulo/stream), a lightweight Dart web server, to the [benchmarking test suite](../). The test is based on the Dart Benchmarking Test.

## Versions

* [Dart SDK version 0.8.7.0_r29341](https://launchpad.net/~hachre/+archive/dart)
* [Dart args version 0.8.7](http://pub.dartlang.org/packages/args)
* [Dart crypto version 0.8.7](http://pub.dartlang.org/packages/crypto)
* [Dart mongo_dart version 0.1.31](http://pub.dartlang.org/packages/mongo_dart)
* [Dart postgresql version 0.2.9](http://pub.dartlang.org/packages/postgresql)
* [Dart stream version 0.8.6+2](http://pub.dartlang.org/packages/start)
* [Dart yaml version 0.8.7](http://pub.dartlang.org/packages/yaml)

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
http://localhost:8080/queries?queries=2

#### Fortunes Test
http://localhost:8080/fortunes

#### Data-Store/Database Update Test
http://localhost:8080/updates

#### Variable Update Test
http://localhost:8080/updates?queries=2


### MongoDB

#### Data-Store/Database Mapping Test
http://localhost:8080/db-mongo

#### Variable Query Test
http://localhost:8080/queries-mongo?queries=2

#### Fortunes Test
http://localhost:8080/fortunes-mongo

#### Data-Store/Database Update Test
http://localhost:8080/updates-mongo

#### Variable Update Test
http://localhost:8080/updates-mongo?queries=2

