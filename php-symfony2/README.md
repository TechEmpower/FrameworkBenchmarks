# Cake PHP Benchmarking Test

This is the Symfony 2 PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](src/Skamander/BenchmarkBundle/BenchController.php)


### Data-Store/Database Mapping Test
Uses the Symfony 2/Doctrine 2 Entity functionality.

* [DB test controller](src/Skamander/BenchmarkBundle/Controller/BenchController.php)
* [DB test model](src/Skamander/BenchmarkBundle/Entity/World.php)


## Infrastructure Software Versions
The tests were run with:

* [Symfony Version 2.2.1](http://symfony.com/)
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
