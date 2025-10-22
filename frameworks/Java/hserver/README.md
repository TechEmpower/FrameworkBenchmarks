# HServer Benchmarking Test
This is the HServer portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### Tests
* [JSON test source](src/main/java/com/test/hserver/controller/TestController.java)
* [Plaintext test source](src/main/java/com/test/hserver/controller/TestController.java)
* [Data-Store test source](src/main/java/com/test/hserver/controller/TestController.java)
* [Data-Update test source](src/main/java/com/test/hserver/controller/TestController.java)
* [Fortunes test source](src/main/java/com/test/hserver/controller/TestController.java)

## Infrastructure Software Versions

* [HServer](https://gitee.com/HServer/HServer)
* [Java OpenJDK 1.8](http://openjdk.java.net/)

## Test URLs

### JSON Encoding Test

http://localhost:8888/json 

### Plain Text Test

http://localhost:8888/plaintext

### Data-Store/Database Mapping Test

http://localhost:8888/db?queries=2

### Update Test

http://localhost:8888/updates?queries=2

### Fortunes Test

http://localhost:8888/fortunes

### Query Test
http://localhost:8888/queries?queries=2
