# Redstone.dart Framework Benchmarking Test

This test adds [Redstone.dart](http://redstonedart.org), a microframework for Dart, to the [benchmarking test suite](../). The test is based on the Dart Benchmarking Test.

## Versions

* [Dart SDK version >=1.7.0](http://www.dartlang.org/)
* [Dart args version 0.10.0+2](http://pub.dartlang.org/packages/args)
* [Dart crypto version 0.9.0](http://pub.dartlang.org/packages/crypto)
* [Dart mustache version 0.1.8](http://pub.dartlang.org/packages/mustache)
* [Dart mongo_dart version 0.1.44](http://pub.dartlang.org/packages/mongo_dart)
* [Dart postgresql version 0.2.14](http://pub.dartlang.org/packages/postgresql)
* [Dart redstone version 0.5.18](http://pub.dartlang.org/packages/redstone)
* [Dart yaml version 2.0.1+1](http://pub.dartlang.org/packages/yaml)
* [Dart redstone_mapper version 0.1.9](http://pub.dartlang.org/packages/redstone_mapper)
* [Dart redstone_mapper_mongo version 0.1.1+1](http://pub.dartlang.org/packages/redstone_mapper_mongo)
* [Dart redstone_mapper_pg version 0.1.1](http://pub.dartlang.org/packages/redstone_mapper_pg)

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


