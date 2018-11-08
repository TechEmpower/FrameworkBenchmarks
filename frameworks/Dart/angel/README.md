# Angel Framework Benchmarking Test
This test adds [Angel](https://angel-dart.github.io),
a full-featured framework for Dart, to the
[benchmarking test suite](https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/frameworks/Dart). The test is based on the Dart Benchmarking Test.

## Versions
The `pubspec.lock` file is included; so that dependencies are kept consistent between deployments.
The tests included in this benchmark are a demonstration of:
* [Dart SDK version 2.0.0](http://www.dartlang.org/)
* [Angel Framework version `^2.0.0-alpha`](https://pub.dartlang.org/packages/angel_framework/versions/2.0.0-alpha.1)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=

### CACHED QUERY

http://localhost:8080/cached_query?queries=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes
