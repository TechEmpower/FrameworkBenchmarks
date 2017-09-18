# Pronghorn
This is the [Pronghorn Http Server](https://www.pronghorn.tech) portion of the [TechEmpower Framework Benchmarks](https://github.com/TechEmpower/FrameworkBenchmarks). 

## About
The Pronghorn HTTP Server is a low-level, high performance HTTP server written in [Kotlin](https://kotlinlang.org/).

### Version
v0.1.0 : [https://github.com/pronghorn-tech/server/releases/tag/0.1.0](https://github.com/pronghorn-tech/server/releases/tag/0.1.0)

## Test Types
Currently only the plaintext and json tests are implemented.

### Plaintext
url : `http://TFB-server:8080/plaintext` 
source code : [TestServer.kt](src/main/kotlin/pronghorn/TestServer.kt)

### Json
url : `http://TFB-server:8080/json`
source code : [JsonHandler.kt](src/main/kotlin/pronghorn/JsonHandler.kt)

## Additional Dependencies
Pronghorn is by default a zero dependency library, but for the purpose of benchmarking these tests utilize three Pronghorn plugins for performance and logging :
* [JCTools Collections Plugin](https://github.com/pronghorn-tech/plugin-collections-jctools) - performance
* [OpenHFT Hashing Plugin](https://github.com/pronghorn-tech/plugin-hashing-openhft) - performance
* [SLF4J Logging Plugin](https://github.com/pronghorn-tech/plugin-logging-slf4j) - logging

In addition to the above plugins, the json test utilizes the [jsoniter](http://jsoniter.com/) library for the actual json implementation, as well as the [Javassist](http://jboss-javassist.github.io/javassist/) library for improved performance of jsoniter. 

## Contact
For additional information, help, or corrections concerning Pronghorn or these tests contact info [at] pronghorn.tech 
