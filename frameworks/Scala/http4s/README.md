# http4s Benchmarking Test

### JSON Encoding Test

* [JSON test source](src/main/scala/code/lib/WebServer.scala)

## Infrastructure Software Versions
The tests were run with:

* [Java Oracle 1.8](http://www.oracle.com/technetwork/java/javase)
* [http4s 0.15.9a](http://http4s.org/)
* [blaze 0.12.4](https://github.com/http4s/blaze/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/json

### Plaintext Test

http://localhost:8080/plaintext

## How to run
sbt 'oneJar'

java -jar target/scala-2.12/http4s_2.12-1.0-SNAPSHOT-one-jar.jar "$DBHOST"
