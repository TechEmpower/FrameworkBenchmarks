#Phreeze Benchmarking Test

This is the Phreeze portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](libs/Controller/TestController.php)

### Data-Store/Database Mapping Test

* [Database test source](libs/Controller/TestController.php)

## Infrastructure Software Versions
The tests were run with:

* [PHP Version 5.3.15](http://www.php.net/)
* [Apache Version 2.2.22](http://httpd.apache.org/)
* [MySQL 5.5.27](https://dev.mysql.com/)
* [Phreeze 3.3.1](http://www.phreeze.com/)

## Test URLs
### JSON Encoding Test

http://localhost/phreeze/json

### Data-Store/Database Mapping Test

http://localhost/phreeze/db

### Variable Query Test

http://localhost/phreeze/db?queries=5