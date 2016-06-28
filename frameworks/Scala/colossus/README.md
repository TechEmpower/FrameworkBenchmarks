#colossus Benchmarking Test

### JSON Encoding Test

* [JSON test source](src/main/scala/example/Main.scala)

### Plaintext Test

* [JSON test source](src/main/scala/example/Main.scala)

## Infrastructure Software Versions
The tests were run with:

* [Java Oracle 1.8.0_25](http://www.oracle.com/technetwork/java/javase)
* [colossus 0.6.1](http://tumblr.github.io/colossus/)

## Test URLs
### JSON Encoding Test

http://localhost:9007/json

### Plaintext Test

http://localhost:9007/plaintext

## How to run
sbt 'oneJar'

java -jar target/scala-2.11/colossus*one-jar.jar

