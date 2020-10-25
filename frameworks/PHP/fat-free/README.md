#PHP Fat-Free Benchmarking Test

This is the PHP Fat-Free Framework portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Test URLs
### JSON Encoding Test

http://localhost/json


### Data-Store/Database Mapping Test

Raw:
http://localhost/db

ORM:
http://localhost/db-orm

### Variable Query Test

Raw:
http://localhost/db/5

ORM:
http://localhost/db-orm/5

## Infrastructure Software Versions
The tests were run with:

* [Fat-Free Version 3.7.1](https://github.com/bcosca/fatfree)
* [PHP Version 7.4](http://www.php.net/) with FPM