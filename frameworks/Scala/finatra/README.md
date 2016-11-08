#finatra Benchmarking Test

### JSON Encoding Test

* [JSON test source](src/main/scala/benchmark/controllers/Controller.scala)

### Plaintext Test

* [JSON test source](src/main/scala/benchmark/controllers/Controller.scala)

## Infrastructure Software Versions
The tests are run with:

* [Java Oracle 1.8.0_25](http://www.oracle.com/technetwork/java/javase)
* [finatra 2.3.0](https://github.com/twitter/finatra/tree/finatra-2.3.0)

## Test URLs
### JSON Encoding Test

http://localhost:8888/json

### Plaintext Test

http://localhost:8888/plaintext

## How to run
sbt 'assembly'

`java -Dcom.twitter.util.events.sinkEnabled=false -Xmx2000M -Xms2000M -Xmn1750M -XX:MetaspaceSize=128M -XX:ParallelGCThreads=4 -XX:+CMSScavengeBeforeRemark -XX:TargetSurvivorRatio=90 -XX:+UseConcMarkSweepGC -XX:CMSInitiatingOccupancyFraction=70 -XX:+UseCMSInitiatingOccupancyOnly -XX:TargetSurvivorRatio=60 -jar target/scala-2.11/finatra-benchmark.jar -log.level=ERROR -http.response.charset.enabled=false`
