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

* [PHP Version 7.0.1](http://www.php.net/) with FPM and APC
* [nginx 1.9.9](http://nginx.org/)
* [MySQL 5.5.29](https://dev.mysql.com/)
* [PHP ActiveRecord Nightly 20121221](http://www.phpactiverecord.org/)

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
