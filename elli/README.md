# Elli Benchmarking Test

This is the Elli portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test controller](src/elli_bench_cb.erl)


### Data-Store/Database Mapping Test

* [DB test controller](src/elli_bench_cb.erl)


## Infrastructure Software Versions
The tests were run with:

* [Elli](git://github.com/knutin/elli)
* [Erlang R16B](http://www.erlang.org/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db

### Variable Query Test
    
http://localhost/db?queries=2
