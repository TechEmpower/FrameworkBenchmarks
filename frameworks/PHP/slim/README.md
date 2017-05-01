# Slim Framework Benchmarking Test

This is the Slim Framework portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](index.php)


### Data-Store/Database Mapping Test
Uses the PDO access layer

* [DB test controller](index.php)


## Infrastructure Software Versions
The tests were run with:

* [Slim 3.3.0](http://www.slimframework.com/)
* [PHP Version 7.1.4](http://www.php.net/) with FPM and OPcache
* [nginx 1.9.9](http://nginx.org/)
* [MySQL 5.5.54](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db

### Variable Query Test
    
http://localhost/db?queries=2
