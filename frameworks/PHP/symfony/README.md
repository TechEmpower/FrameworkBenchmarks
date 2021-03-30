# Symfony 4 Benchmarking Test

This is the Symfony 4 portion of a [benchmarking test suite](../) comparing a variety of web development platforms.


### Plaintext Test
* [Plaintext test controller](src/Controller/PlaintextController.php)

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](src/Controller/JsonController.php)

### Data-Store/Database Mapping Test
Uses Doctrine ORM 2.6.

* [DB ORM test controller](src/Controller/DbController.php)

Uses Doctrine DBAL 2.9.

* [DB SQL test controller](src/Controller/DbRawController.php)

### Template Test
Uses Twig 2.11.

* [Template test controller](src/Controller/FortunesController.php)


## Infrastructure Software Versions
The tests were run with:

* [Symfony](https://symfony.com/)
* [PHP](https://www.php.net/) with FPM and OPcache
* [nginx ](https://nginx.org/)
* [MySQL](https://dev.mysql.com/)


## Test URLs
### JSON Encoding Test
http://localhost/json

### Data-Store/Database Mapping Test
http://localhost/db

### Variable Query Test
http://localhost/queries?queries=2

### Templating Test
http://localhost/fortunes

### Update Test
http://localhost/updates?queries=2

### Plain Text Test
http://localhost/plaintext
