# Symfony 4 Benchmarking Test

This is the Symfony 4 portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](src/Controller/BenchController.php)


### Data-Store/Database Mapping Test
Uses Doctrine ORM 2.6.

* [DB test controller](src/Controller/BenchController.php)

### Template Test
Uses Twig 2.11.

* [Template test controller](src/Controller/BenchController.php)


## Infrastructure Software Versions
The tests were run with:

* [Symfony 4.3](https://symfony.com/)
* [PHP Version 7.3](https://www.php.net/) with FPM and OPcache
* [nginx 1.15](https://nginx.org/)
* [MySQL 5.7](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db

### Variable Query Test
    
http://localhost/db?queries=2

### Templating Test

http://localhost/fortunes

### Update Test

http://localhost/update?queries=2

### Plain Text Test

http://localhost/plaintext
