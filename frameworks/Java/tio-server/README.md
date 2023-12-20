# t-io Benchmarking Test

This is the tio-server portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Controller

These implementations use the tio-server's controller.
* [Plaintext test source]()
* [JSON test source]()

### Plaintext Test

* [Plaintext test source](src/main/java/com/litongjava/tio/http/server/controller/IndexController.java)

### JSON Serialization Test

* [JSON test source](src/main/java/com/litongjava/tio/http/server/controller/IndexController.java)

### Database Query Test

* [Database Query test source](src/main/java/com/litongjava/tio/http/server/controller/DbController.java))

### Database Queries Test

* [Database Queries test source](src/main/java/com/litongjava/tio/http/server/controller/DbController.java))

### Database Update Test

* [Database Update test source](src/main/java/com/litongjava/tio/http/server/controller/DbController.java))

### Template rendering Test

* [Template rendering test source](src/main/java/com/litongjava/tio/http/server/controller/DbController.java))

### Cache Query Test
* [Cache query test source](src/main/java/com/litongjava/tio/http/server/controller/CacheController.java))


## Versions
3.7.3.v20231218-RELEASE (https://gitee.com/litongjava/t-io)

## Test URLs

All implementations use the same URLs.

### Plaintext Test

    http://localhost:8080/plaintext

### JSON Encoding Test

    http://localhost:8080/json

### Database Query Test

    http://localhost:8080/db

### Database Queries Test

    http://localhost:8080/queries?queries=5

### Cache Query Test

    http://localhost:8080/cacheQuery?queries=10000

### Template rendering Test

    http://localhost:8080/fortunes
    
### Database Update Test

    http://localhost:8080/updates?queries=5

 ## Hot to run
 ```
 docker build -t tio-server-benchmark -f tio-server.dockerfile .
```

```
docker run --rm -p 8080:8080 tio-server-benchmark
```

The run is to specify the mysql database
**java**
-windows
```
D:\java\jdk1.8.0_121\bin\java -jar target\tio-server-benchmark-1.0.jar --jdbc.url=jdbc:mysql://192.168.3.9/hello_world --jdbc.user=root --jdbc.pswd=robot_123456#
```
**docker**

```
docker run --rm -p 8080:8080 tio-server-benchmark --jdbc.url=jdbc:mysql://192.168.3.9/hello_world --jdbc.user=root --jdbc.pswd=robot_123456#
```
