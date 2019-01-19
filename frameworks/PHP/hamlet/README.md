# Hamlet Framework Benchmarking Test

This is the [Hamlet Core](https://github.com/vasily-kartashov/hamlet-core) portion of a [benchmarking test suite](../)

Current submission tests the following configurations:

- Hamlet + PHP FPM
- Hamlet + ReactPHP
- Hamlet + RoadRunner
- Hamlet + Swoole

## (Some) Documentation

- [Short overview of framework's architecture](https://notes.kartashov.com/2016/07/08/simple-caching-web-framework/)
- [Data processor](https://notes.kartashov.com/2017/05/09/result-set-processor/)

## Dependency

```json
{
  ...
  "require": {
    "hamlet/hamlet-core" : "^3"
    ...
  }
}
```

## Local test

ab -c 1 -n 100 "http://127.0.0.1:8089/json"
