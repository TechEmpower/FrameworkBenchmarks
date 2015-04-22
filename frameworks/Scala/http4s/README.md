#http4s Benchmarking Test

### JSON Encoding Test

* [JSON test source](src/main/scala/code/lib/WebServer.scala)

## Infrastructure Software Versions
The tests were run with:

* [Java Oracle 1.8.0_25](http://www.oracle.com/technetwork/java/javase)
* [http4s 0.6.2](http://http4s.org/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/json

## How to run
sbt package

java -jar target/scala-2.11/http4s_2.11-1.0-SNAPSHOT-one-jar.jar