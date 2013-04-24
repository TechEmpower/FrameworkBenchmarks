# Phalcon PHP Benchmarking Test

This is the Phalcon PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](app/controllers/BenchController.php)


### Data-Store/Database Mapping Test
Uses the built-in ORM of Phalcon PHP

* [DB test controller](app/controllers/BenchController.php)


## Infrastructure Software Versions
The tests were run with:

* [Phalcon 1.0.0](http://phalconphp.com/)
* [PHP Version 5.4.13](http://www.php.net/) with FPM, APC and Phalcon extension
* [nginx 1.4.0](http://nginx.org/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db

### Variable Query Test
    
http://localhost/db?queries=2
