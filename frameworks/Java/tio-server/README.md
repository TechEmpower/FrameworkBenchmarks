# t-io Benchmarking Test

This is the tio-server portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Controller

These implementations use the tio-server's controller.
* [Plaintext test source](src/main/java/com/litongjava/tio/http/server/controller/IndexController.java)
* [JSON test source](src/main/java/com/litongjava/tio/http/server/controller/IndexController.java)


## Versions
3.7.3.v20231218-RELEASE (https://gitee.com/litongjava/t-io)

## Test URLs

All implementations use the same URLs.

 * Plaintext - `http://localhost:8080/plaintext`
 * JSON - `http://localhost:8080/json`
 
 ## Hot to run
 ```
 docker build -t tio-server-benchmark -f tio-server.dockerfile .
```

```
docker run --rm -p 8080:8080 tio-server-benchmark
```