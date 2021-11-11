# libhttpserver
Only the plaintext test is implemented.

## What is libhttpserver
libhttpserver is a Free High-Performance library aimed at rapid development of embedded C++ servers:

- It is designed and tuned to handle extremely high loads (tested with > 10k connections).
- It uses modern C++ as the primary development language in order to achieve the first goal.
- It is designed with REST as a primary design principle

It is available under open source LGPLv2 license.

libhttpserver lives here: https://github.com/etr/libhttpserver

# libhttpserver Benchmarking Test

### Test Type Implementation Source Code

* [PLAINTEXT](benchmark.cpp)

## Important Libraries
libhttpserver uses libmicrohttpd as base library:
* [libmicrohttpd](https://www.gnu.org/software/libmicrohttpd/)

## Test URLs
### PLAINTEXT

http://localhost:8080/plaintext

## Links
[Homepage](https://github.com/etr/libhttpserver)
