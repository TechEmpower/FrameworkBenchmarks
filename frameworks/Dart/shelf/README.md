## Shelf Benchmarking Test

### Test Type Implementation Source Code

setup:

- [SERVER](lib/server.dart)
- [SERVER](lib/src/multithread.dart.dart)

benchmarks:

- [JSON](lib/src/json.dart)
- [PLAINTEXT](lib/src/plain_text.dart)

### Important Libraries

The tests were run with:

- [args](https://pub.dev/packages/args)
- [shelf](https://pub.dev/packages/shelf)
- [shelf_router](https://pub.dev/packages/shelf_router)


### Test URLs

- json: http://localhost:8080/json
- plain text: http://localhost:8080/plainText
