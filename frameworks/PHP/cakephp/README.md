# Cake PHP Benchmarking Test

This is the Cake PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](app/Controller/JsonController.php)


### Data-Store/Database Mapping Test
Uses the CakePHP Model functionality.

* [DB test controller](app/Controller/WorldController.php)
* [DB test model](app/Model/World.php)


## Infrastructure Software Versions
The tests were run with:

* [Cake Version 2.10.4](https://cakephp.org/)
* [PHP Version 5.6.30](http://www.php.net/) with FPM and APC
* [PHP Version 7.1.4](http://www.php.net/) with FPM and Opcache
* [nginx 1.12.0](http://nginx.org/)
* [MySQL 5.7.19](https://dev.mysql.com/)

Cake Debug mode is set to 0 in [core.php](app/Config/core.php), as
appropriate for a production deployment.

To support the Cake JsonView and use the recommended URLs, 
we also made a [routes configuration change](app/Config/routes.php).

## Test URLs
### JSON Encoding Test

http://localhost/index.php/json

### Data-Store/Database Mapping Test

http://localhost/index.php/db

### Variable Query Test
    
http://localhost/index.php/queries?queries=2

### Variable Update Test

http://localhost/index.php/updates?queries=2
