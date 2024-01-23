# Dart Start Framework Benchmarking Test

## Note:
The PostgreSQL tests are no longer working. The implementation is still in the framework folder but has been removed from the `benchmark_config.json` file.

This test adds [Start](https://github.com/lvivski/start), a Sinatra inspired web development framework for Dart, to the [benchmarking test suite](../). The test is based on the Dart Benchmarking Test.

## Versions

* [Dart SDK version >=1.3.0](https://launchpad.net/~hachre/+archive/dart)
* [Dart args version 0.11.0+1](https://pub.dev/packages/args)
* [Dart crypto version 0.9.0](https://pub.dev/packages/crypto)
* [Dart mustache version 0.1.8](https://pub.dev/packages/mustache)
* [Dart mongo_dart version 0.1.39](https://pub.dev/packages/mongo_dart)
* [Dart postgresql version 0.2.13](https://pub.dev/packages/postgresql)
* [Dart start version 0.2.4](https://pub.dev/packages/start)
* [Dart yaml version 0.9.0](https://pub.dev/packages/yaml)

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


