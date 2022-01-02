# Angel3 Framework Benchmarking Test

This is the Angel3 framework portion of a [benchmarking test suite](../) comparing a variety of web development platforms. 

## Description

All the tests are implemented using the [Angel3 Framework](https://angel3-framework.web.app) with ORM for Postgresql database enabled. The directory layout follows the standard ORM boilerplate template.

### Test Type Implementation Source Code

* [JSON](orm/lib/src/routes/controllers/controllers.dart)
* [PLAINTEXT](orm/lib/src/routes/controllers/controllers.dart)
* [DB](orm/lib/src/routes/controllers/controllers.dart)
* [QUERY](orm/lib/src/routes/controllers/controllers.dart)
* [UPDATE](orm/lib/src/routes/controllers/controllers.dart)
* [FORTUNES](orm/lib/src/routes/controllers/controllers.dart)
* [FORTUNES VIEW TEMPLATE](orm/views/listing.jael)

## Important Libraries

The tests were run with:

* [Dart](https://dart.dev/get-dart)
* [Angel3 Framework](https://angel3-framework.web.app)
* [Example](https://angel3-framework.web.app/#/examples)

## Test URLs

### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes
