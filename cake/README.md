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

* [Cake Version 2.3.0](http://cakephp.org/)
* [PHP Version 5.4.13](http://www.php.net/) with FPM and APC
* [nginx 1.2.7](http://nginx.org/)
* [MySQL 5.5.29](https://dev.mysql.com/)

Cake Debug mode is set to 0 in [core.php](app/Config/core.php), as
appropriate for a production deployment.

To support the Cake JsonView, we also made a [routes configuration change](app/Config/routes.php).

## Test URLs
### JSON Encoding Test

http://localhost/index.php/json.json

### Data-Store/Database Mapping Test

http://localhost/index.php/world.json

### Variable Query Test
    
http://localhost/index.php/world.json?queries=2
