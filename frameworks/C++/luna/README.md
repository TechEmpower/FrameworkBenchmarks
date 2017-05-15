#Luna Benchmarking Test

[Luna](https://github.com/DEGoodmanWilson/luna) is a web framework for modern C++, designed for low latency and high throughput. These tests use version 2.10.1 of Luna.

### Source Code

* [Default threading mode, with thread pool](default.cpp)
* [epoll threading mode (linux only)](epoll.cpp)
* [Thread-per-connection mode](thread.cpp)

### Test URLs

[/plaintext](http://www.techempower.com/benchmarks/#section=plaintext)
----------
```
HTTP/1.1 200 OK
Connection: Keep-Alive
Content-Length: 13
Server: luna/2.10.1
Content-Type: text/plain
Server: luna
Date: Thu, 13 Apr 2017 11:40:58 GMT

Hello, world!
```


[/json](http://www.techempower.com/benchmarks/#section=json)
----------
```
HTTP/1.1 200 OK
Connection: Keep-Alive
Content-Length: 27
Server: luna/2.10.1
Content-Type: application/json
Server: luna
Date: Thu, 13 Apr 2017 11:40:58 GMT

{"message":"Hello, World!"}
```
