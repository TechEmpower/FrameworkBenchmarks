# Finch Benchmark

Implements two endpoints:

 - `GET /plaintext`
 - `GET /json`

## Run Locally (using `sbt`)

```
$ sbt assembly
$ java -jar target/scala-2.12/finch-benchmark.jar
```

## Test Locally (using `httpie`)

```
$ http :9000/plaintext
$ http :9000/json
```