# Micromvc Benchmarking Test

This is the Micromvc portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](Class/Controller/Benchmark/Json.php)


### Data-Store/Database Mapping Test
Uses the built-in ORM of micromvc

* [DB test controller](Class/Controller/Benchmark/Db.php)


## Infrastructure Software Versions
The tests were run with:

* [Micromvc 4.0.0](http://www.micromvc.com/)
* [PHP Version 5.4.13](http://www.php.net/) with FPM and APC
* [nginx 1.2.7](http://nginx.org/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db

### Variable Query Test
    
http://localhost/db?queries=2
