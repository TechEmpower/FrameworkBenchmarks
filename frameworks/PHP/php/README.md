# PHP Benchmarking Test

This is the PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### Important
When editing this framework, be sure to force add the files changed. Most files were added to .gitignore, as the framework touches some of them during testing.

## Infrastructure Software Versions
The tests were run with [PHP Version 7.3.*](https://www.php.net/) + OPcache

### Platforms

* [php-fpm 7.4.*](https://www.php.net/manual/en/install.fpm.php)
* [nginx unit 1.18.0](https://unit.nginx.org/) (PHP 7.4)

### Webservers

* [nginx 1.17.10](https://nginx.org/)
* [h2o 2.2.6](https://h2o.examp1e.net/)

### Databases

* [MySQL 8](https://dev.mysql.com/)
* [PostgreSQL 12](https://www.postgresql.org/)

## ActiveRecord

* [PHP ActiveRecord Nightly 20121221](http://www.phpactiverecord.org/)

## Eloquent ORM / Laravel Query Builder

* [illuminate/database 5.8](https://github.com/illuminate/database)

The Laravel query builder / eloquent ORM components can be run separately from the Laravel framework.
([Instructions](https://github.com/illuminate/database)) This is used for two tests:

* php-laravel-query-builder: uses the [Laravel query builder](https://laravel.com/docs/5.8/queries) without using the full Eloquent ORM.  
This tests the overhead of the dependency injection container ([Capsule](https://github.com/illuminate/database/blob/master/Capsule/Manager.php)) 
and the query builder without the overhead of the Eloquent ORM models. 
* php-eloquent: uses the [Laravel Eloquent ORM](https://laravel.com/docs/5.8/eloquent) which is built on top of the
Laravel query builder.  This tests the incremental overhead of the Eloquent ORM models.

These tests measure overhead compared to a PHP-PDO implementation.  Comparing these tests to the Laravel framework results
also illustrates the overhead of the Laravel frontend components relative to the backend components. 

[Brion Finlay](https://github.com/bfinlay)

## Test URLs
### JSON Encoding Test

http://localhost/json.php

### Data-Store/Database Mapping Test

Raw:
http://localhost/dbraw.php

ORM:
http://localhost/dborm.php

### Variable Query Test

Raw:
http://localhost/dbraw.php?queries=5

ORM:
http://localhost/dborm.php?queries=5

### Eloquent ORM / Laravel Query Builder tests
#### Eloquent ORM
* db: http://localhost/eloquent/db-eloquent.php,
* query: http://localhost/eloquent/db-eloquent.php?queries=x
* update: http://localhost/eloquent/update-eloquent.php?queries=x
* fortune: http://localhost/eloquent/fortune-eloquent.php
#### Laravel Query Builder
* db: http://localhost/eloquent/db-laravel-query-builder.php
