# Slim Framework Benchmarking Test

This is the Slim Framework portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](index.php)


### Data-Store/Database Mapping Test
Uses the RedBeanPHP ORM

* [DB test controller](index.php)


## Infrastructure Software Versions
The tests were run with:

* [Slim 2.2.0](http://www.slimframework.com/)
* [RedBeanPHP 3.4.2](http://redbeanphp.com/)
* [PHP Version 5.5.17](http://www.php.net/) with FPM and APC
* [nginx 1.4.0](http://nginx.org/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db

### Variable Query Test
    
http://localhost/db?queries=2
