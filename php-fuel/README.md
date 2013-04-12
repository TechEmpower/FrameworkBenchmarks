# FuelPHP Benchmarking Test

This is the FuelPHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](fuel/app/classes/controllers/bench.php)


### Data-Store/Database Mapping Test
Uses the Fuel ORM

* [DB test controller](fuel/app/classes/controllers/bench.php)


## Infrastructure Software Versions
The tests were run with:

* [FuelPHP 1.5.3](http://fuelphp.com/)
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
