# Kohana PHP Benchmarking Test

This is the Kohana PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](application/classes/Controller/Bench.php)


### Data-Store/Database Mapping Test
Uses the db abstraction class from Kohana

* [DB test controller](application/classes/Controller/Bench.php)


## Infrastructure Software Versions
The tests were run with:

* [Codeigniter Version 2.1.3](http://kohanaframework.org/)
* [PHP Version 5.4.13](http://www.php.net/) with FPM and APC
* [nginx 1.2.7](http://nginx.org/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost/index.php/bench/json

### Data-Store/Database Mapping Test

http://localhost/index.php/bench/db

### Variable Query Test
    
http://localhost/index.php/bench/db/2
