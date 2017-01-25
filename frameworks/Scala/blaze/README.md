#blaze Benchmarking Test

## Infrastructure Software Versions
The tests were run with:

* [Java Oracle 1.8.0_101](http://www.oracle.com/technetwork/java/javase)
* [blaze 0.13.0](https://github.com/http4s/blaze/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/json

### Plaintext Test

http://localhost:8080/plaintext

## How to run
sbt 'oneJar'

java -jar target/scala-2.11/blaze_2.11-1.0-SNAPSHOT-one-jar.jar
