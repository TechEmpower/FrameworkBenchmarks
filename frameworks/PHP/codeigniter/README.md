# Codeigniter PHP Benchmarking Test

This is the Codeigniter PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](application/controllers/bench.php)


### Data-Store/Database Mapping Test
Uses the db abstraction class from Codeigniter

* [DB test controller](application/controllers/bench.php)


## Infrastructure Software Versions
The tests were run with:

* [Codeigniter Version 3.1.11](http://ellislab.com/codeigniter)
* [PHP Version 7.4](http://www.php.net/) with FPM and APC
* [nginx 1.16.1](http://nginx.org/)
* [MySQL 8](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost/index.php/bench/json

### Data-Store/Database Mapping Test

http://localhost/index.php/bench/db

### Variable Query Test
    
http://localhost/index.php/bench/db/2
