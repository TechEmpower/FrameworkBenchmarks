# Mana Benchmarking Test

[Mana](https://github.com/includeos/mana) is a web framework for modern C++, designed to be run on the IncludeOS unikernel.

### Features

* Serve static content from a disk with just a few lines of code.
* RESTful API service reading and posting JSON.
* Real-time information about the server with an interactive Dashboard


### Test URLs

[/plaintext](http://www.techempower.com/benchmarks/#section=plaintext)
----------
```
HTTP/1.1 200 OK
Server: IncludeOS/v0.11.0
Content-Type: text/plain
Date: Mon, 01 Jan 1970 00:00:01 GMT
Content-Length: 13

Hello, world!
```


### TODO

* Initial test run will fail (apart from on Travis CI) due to docker failing to run as non-root user until session is logged out and back in.
* Travis run will fail at "Building IncludeOS Mana server" step (CMake file does not exist error)
* IncludeOS-Mana eventually becomes unresponsive after benchmark has been running for >10sec and QEMU CPU will be 100%
