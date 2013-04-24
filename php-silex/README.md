# Silex Benchmarking Test

This is the Silex PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](web/index.php)


### Data-Store/Database Mapping Test
Uses the Doctrine DBAL functionality.

* [DB test controller](web/index.php)


## Infrastructure Software Versions
The tests were run with:

* [Silex Version 1.0](http://silex.sensiolabs.org/)
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
