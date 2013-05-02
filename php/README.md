#PHP Benchmarking Test

This is the PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Use the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php)

* [JSON test source](json.php)

### Data-Store/Database Mapping Test

* [Database test source Raw](dbraw.php)
* [Database test source ORM](dborm.php)

## Infrastructure Software Versions
The tests were run with:

* [PHP Version 5.4.13](http://www.php.net/) with FPM and APC
* [nginx 1.4.0](http://nginx.org/)
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
