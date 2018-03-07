[![KumbiaPHP logo](https://rawgit.com/kumbiaphp/kumbiaphp/1.0/default/public/img/kumbiaphp.svg)](https://github.com/KumbiaPHP/KumbiaPHP) 

[![Scrutinizer Code Quality](https://scrutinizer-ci.com/g/KumbiaPHP/KumbiaPHP/badges/quality-score.png?b=1.0)](https://scrutinizer-ci.com/g/KumbiaPHP/KumbiaPHP/?branch=1.0)
[![Code Climate](https://codeclimate.com/github/KumbiaPHP/KumbiaPHP/badges/gpa.svg)](https://codeclimate.com/github/KumbiaPHP/KumbiaPHP)
![PHP7 ready](https://rawgit.com/kumbiaphp/kumbiaphp/1.0/default/public/img/php7.svg)

# KumbiaPHP Benchmarking Test

This is the KumbiaPHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](bench/app/controllers/json_controller.php)


### Data-Store/Database Mapping Test

* [DB test controller](bench/app/controllers/db_controller.php)


## Infrastructure Software Versions
The tests were run with:

* [KumbiaPHP Version 1.0](https://github.com/KumbiaPHP/KumbiaPHP)
* [PHP Version 7.2.22](http://www.php.net/) with FPM and OPCache
* [nginx 1.12.2](http://nginx.org/)
* [MySQL 5.7.21](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db/

### Variable Query Test
    
http://localhost/db/queries/2

