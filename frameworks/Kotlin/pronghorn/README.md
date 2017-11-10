# Pronghorn
This is the [Pronghorn Http Server](https://www.pronghorn.tech) portion of the [TechEmpower Framework Benchmarks](https://github.com/TechEmpower/FrameworkBenchmarks). 

## About
The Pronghorn HTTP Server is a low-level, high performance HTTP server written in [Kotlin](https://kotlinlang.org/).

### Version
v0.1.2 : [https://github.com/pronghorn-tech/server/releases/tag/0.1.2](https://github.com/pronghorn-tech/server/releases/tag/0.1.2)

## Test Types

### Plaintext
url : `http://TFB-server:8080/plaintext` 
source code : [TestServer.kt](src/main/kotlin/pronghorn/TestServer.kt)

### Json Serialization
url : `http://TFB-server:8080/json`
source code : [JsonHandler.kt](src/main/kotlin/pronghorn/handlers/JsonHandler.kt)

### Single Query
url : `http://TFB-server:8080/db`
source code : [MongoDBRequestSingleHandler.kt](src/main/kotlin/pronghorn/handlers/MongoDBRequestSingleHandler.kt)

### Multiple Query
url : `http://TFB-server:8080/queries`
source code : [MongoDBRequestMultiHandler.kt](src/main/kotlin/pronghorn/handlers/MongoDBRequestMultiHandler.kt)

### Data Updates
url : `http://TFB-server:8080/updates`
source code : [MongoDBRequestUpdatesHandler.kt](src/main/kotlin/pronghorn/handlers/MongoDBRequestUpdatesHandler.kt)

### Fortunes
url : `http://TFB-server:8080/fortunes`
source code : [MongoDBRequestFortunesHandler.kt](src/main/kotlin/pronghorn/handlers/MongoDBRequestFortunesHandler.kt)

## Additional Dependencies

### Pronghorn Plugins
Pronghorn is by default a zero dependency library, but for the purpose of benchmarking these tests utilize three Pronghorn plugins for performance and logging :
* [JCTools Collections Plugin](https://github.com/pronghorn-tech/plugin-collections-jctools) - performance
* [OpenHFT Hashing Plugin](https://github.com/pronghorn-tech/plugin-hashing-openhft) - performance
* [SLF4J Logging Plugin](https://github.com/pronghorn-tech/plugin-logging-slf4j) - logging

Additionally, database driven tests utilize the [Pronghorn MongoDB Driver Stream](https://github.com/pronghorn-tech/mongodb-driver-stream) which implements the MongoDB Driver's Stream interface via the [Pronghorn Coroutine Framework](https://github.com/pronghorn-tech/coroutines).  This utilizes the cooperative nature of concurrency in Pronghorn to enable efficient multiplexing of database communication.

### Third-Party Libraries
Beyond the Pronghorn plugins above, these tests utilize several third party libraries.

#### JsonIter
Tests requiring json encoding utilize the [jsoniter](http://jsoniter.com/) library, as well as the [Javassist](http://jboss-javassist.github.io/javassist/) library for improved performance of jsoniter.

#### MongoDB Async Driver 
Database tests depend on the [async MongoDB driver](https://github.com/mongodb/mongo-java-driver/tree/master/driver-async).

#### HTTL
The Fortunes test utilizes the [httl library](http://httl.github.io/en/) as the template engine.

## Contact
For additional information, help, or corrections concerning Pronghorn or these tests contact info [at] pronghorn.tech 
