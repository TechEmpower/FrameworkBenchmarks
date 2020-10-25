# [Gemini](https://github.com/TechEmpower/gemini) Benchmarking Test

This is the [Gemini](https://github.com/TechEmpower/gemini) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](frameworks/Java/gemini/servlet/src/main/java/hello/home/handler/HelloHandler.java)

### Data-Store/Database Mapping Test

* [DB test controller](frameworks/Java/gemini/servlet/src/main/java/hello/home/handler/HelloHandler.java)
* [DB test model](frameworks/Java/gemini/servlet/src/main/java/hello/home/entity/World.java)


## Infrastructure Software Versions
The tests were run with:
* [OpenJDK Java 10](http://jdk.java.net/10/)
* [Resin 4.0.56](http://www.caucho.com/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/

### Data-Store/Database Mapping Test

http://localhost:8080/db

### Variable Query Test

http://localhost:8080/db?queries=2
