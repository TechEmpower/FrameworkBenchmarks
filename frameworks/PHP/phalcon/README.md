# Phalcon PHP Benchmarking Test

This is the Phalcon PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](app/controllers/BenchController.php)


### Data-Store/Database Mapping Test
Uses the built-in ORM/ODM of Phalcon PHP

* MySQL: [DB test controller](app/controllers/BenchController.php)
* MongoDB: [DB test controller](app/controllers/MongobenchController.php)

### Template Test
Uses Phalcons template engine 'Volt'

* [Template test controller](app/controllers/BenchController.php)


## Infrastructure Software Versions
The tests were run with:

* [Phalcon 3.3.1](http://phalconphp.com/)
* [PHP Version 7.2.2](http://www.php.net/) with FPM, OPcache and Phalcon extension
* [nginx 1.12.2](http://nginx.org/)
* [MySQL 5.7.21](https://dev.mysql.com/)
* [MongoDB 3.6](https://mongodb.org/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

MySQL: http://localhost/db
MongoDB: http://localhost/mongodb/db

### Variable Query Test
    
MySQL: http://localhost/queries?queries=2
MongoDB: http://localhost/mongodb/queries?queries=2

### Update Test
    
MySQL: http://localhost/update

http://localhost/fortunes

### Plaintext Test

http://localhost/plaintext

### Fortunes Test
    
MySQL: http://localhost/fortunes
MongoDB: http://localhost/mongodb/fortunes
