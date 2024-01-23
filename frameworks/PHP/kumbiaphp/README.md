[![KumbiaPHP logo](https://rawgit.com/kumbiaphp/kumbiaphp/master/default/public/img/kumbiaphp.svg)](https://github.com/KumbiaPHP/KumbiaPHP) 

[![Scrutinizer Code Quality](https://scrutinizer-ci.com/g/KumbiaPHP/KumbiaPHP/badges/quality-score.png)](https://scrutinizer-ci.com/g/KumbiaPHP/KumbiaPHP/?branch=1.0)
[![Code Climate](https://codeclimate.com/github/KumbiaPHP/KumbiaPHP/badges/gpa.svg)](https://codeclimate.com/github/KumbiaPHP/KumbiaPHP)
![PHP7 ready](https://rawgit.com/kumbiaphp/kumbiaphp/master/default/public/img/php7.svg)

# KumbiaPHP Benchmarking Test

This is the [KumbiaPHP framework](https://kumbiaphp.com) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Infrastructure Software Versions
The tests were run with:

* [KumbiaPHP Version 1.1.1](https://github.com/KumbiaPHP/KumbiaPHP)
* [PHP Version 7.4.*](http://www.php.net/)
* [nginx 1.17.10](http://nginx.org/)
* [MySQL 8](https://dev.mysql.com/)

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](bench/app/controllers/json_controller.php)

### Data-Store/Database Mapping Test

* [DB test controller](bench/app/controllers/db_controller.php)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db/

### Variable Query Test
    
http://localhost/db/queries/2
