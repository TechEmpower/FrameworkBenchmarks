#PHP Benchmarking Test

This is the PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### Important
When editing this framework, be sure to force add the files changed. Most files were added to .gitignore, as the framework touches some of them during testing.

### JSON Encoding Test
Use the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php)

* [JSON test source](json.php)

### Data-Store/Database Mapping Test

* [Database test source Raw](dbraw.php)
* [Database test source ORM](dborm.php)

## Infrastructure Software Versions
The tests were run with:

* [PHP Version 7.3.2](http://www.php.net/) with FPM and OPcache
* [nginx 1.15.5](http://nginx.org/)
* [MySQL 5.7.25](https://dev.mysql.com/)

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
