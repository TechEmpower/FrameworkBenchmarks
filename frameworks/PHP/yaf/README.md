# YAF PHP Benchmarking Test

This is the YAF PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Use the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php)

* [Test RawController::jsonAction()](app/modules/Bench/controllers/Raw.php)

### Data-Store/Database Mapping Test

* [Test RawController::dbAction()](app/modules/Bench/controllers/Raw.php)

### Template (Fortunes) Test

* [Test RawController::fortunesAction()](app/modules/Bench/controllers/Raw.php)

## Infrastructure Software Versions

The tests were run with:

* [YAF 2.2.9](http://www.yafdev.com/)
* [PHP Version 5.4.14](http://www.php.net/) with FPM, APC and YAF extension
* [nginx 1.2.8](http://nginx.org/)
* [MySQL 5.5.31](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/bench/raw/json

### Data-Store/Database Mapping Test

http://localhost:8080/bench/raw/db

### Variable Query Test

http://localhost:8080/bench/raw/db?queries=5

### Fortunes (Template) Test

http://localhost:8080/bench/raw/fortunes