#finch Benchmarking Test

### JSON Encoding Test

* [JSON test source](src/main/scala/example/Main.scala)

### Plaintext Test

* [JSON test source](src/main/scala/example/Main.scala)

## Infrastructure Software Versions
The tests were run with:

* [Java Oracle 1.8.0_25](http://www.oracle.com/technetwork/java/javase)
* [finch 0.7.0](https://github.com/finagle/finch)

## Test URLs
### JSON Encoding Test

http://localhost:9000/json

### Plaintext Test

http://localhost:9000/plaintext

## How to run
sbt 'oneJar'

`java -jar target/scala-2.11/*finch*one-jar.jar`

