# Phalcon PHP Micro Benchmarking Test

This is the Phalcon Micro PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](public/index.php)


### Data-Store/Database Mapping Test
Uses Phalcon\DB component

* [DB test controller](public/index.php)

### Template Test
Uses Phalcon's template engine 'Volt'

* [Template test controller](public/index.php)


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

### Templating Test

http://localhost/fortunes
