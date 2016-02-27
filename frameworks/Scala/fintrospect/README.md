#fintrospect Benchmarking Test

## Infrastructure Software Versions
The tests were run with:

* [Java Oracle 1.8.0_25](http://www.oracle.com/technetwork/java/javase)
* [fintrospect 12.4.0](https://github.com/daviddenton/fintrospect)
* [finagle 6.33.0](https://github.com/twitter/finagle)

## Test URLs

### JSON Encoding Test
http://localhost:9000/json

### Plaintext Test
http://localhost:9000/plaintext

## How to run
sbt 'oneJar'

`java -jar target/scala-2.11/*fintrospect*one-jar.jar`

