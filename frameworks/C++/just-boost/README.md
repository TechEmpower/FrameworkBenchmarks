# Just.Boost Benchmarking Test

## Description

Backend using just C++(20) and Boost.

## Run Test

    cd FrameworkBenchmarks/
    ./tfb --mode verify --test just-boost

## Software Versions

- [Alpine 3.18](https://hub.docker.com/_/alpine)
- [gcc](https://gcc.gnu.org/)
- [c++20](https://en.cppreference.com/w/cpp/20)
- [Boost](https://www.boost.org/)
	- [Beast](https://www.boost.org/doc/libs/1_83_0/libs/beast/doc/html/index.html) ([HTTP Server with C++ 20 coroutine](https://www.boost.org/doc/libs/1_83_0/libs/beast/example/http/server/awaitable/http_server_awaitable.cpp))
	- [JSON](https://www.boost.org/doc/libs/1_83_0/libs/json/doc/html/index.html)
- [libpq — C Library](https://www.postgresql.org/docs/current/libpq.html) (PostgreSQL client)

## Test URLs

### Test 1: JSON Encoding

    http://localhost:8000/json

### Test 2: Single Row Query

    http://localhost:8000/db

### Test 3: Multi Row Query

    http://localhost:8000/queries/{count}

### Test 6: Plaintext

    http://localhost:8000/plaintext
