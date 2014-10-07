# Cowboy Benchmarking Test

This is the Cowboy portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test controller](src/json_handler.erl)


### Data-Store/Database Mapping Test
Uses the db abstraction class from Kohana

* [DB test controller](src/db_handler.erl)


## Infrastructure Software Versions
The tests were run with:

* [Cowboy 0.8.3](https://github.com/extend/cowboy)
* [Erlang R16B](http://www.erlang.org/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db

### Variable Query Test
    
http://localhost/db?queries=2
