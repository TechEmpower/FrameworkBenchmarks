#finatra Benchmarking Test

### JSON Encoding Test

* [JSON test source](src/main/scala/benchmark/controllers/Controller.scala)

### Plaintext Test

* [JSON test source](src/main/scala/benchmark/controllers/Controller.scala)

## Infrastructure Software Versions
The tests are run with:

* [Java Oracle 1.8.0_25](http://www.oracle.com/technetwork/java/javase)
* [finatra 2.2.0](https://github.com/twitter/finatra/tree/finatra-2.2.0)

## Test URLs
### JSON Encoding Test

http://localhost:8888/json

### Plaintext Test

http://localhost:8888/plaintext

## How to run
sbt 'assembly'

`java -jar target/scala-2.11/finatra-benchmark.jar -log.level=ERROR`
