#PHP Benchmarking Test

This is the PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.
NGINX is removed in this test, and substituted by [Workerman, An asynchronous event driven PHP framework](https://github.com/walkor/Workerman). An asynchronous event driven PHP framework for easily building fast, scalable network applications. Supports HTTP, Websocket, SSL and other custom protocols. Supports libevent, HHVM, ReactPHP.

https://github.com/walkor/Workerman

### Important
When editing this framework, be sure to force add the files changed. Most files were added to .gitignore, as the framework touches some of them during testing.

### JSON Encoding Test
Use the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php)

* [JSON test source](server.php)

### Data-Store/Database Mapping Test

* [Database test source Raw](dbraw.php)

## Infrastructure Software Versions
The tests were run with:

* [PHP Version 7.1.9](http://www.php.net/)
* [MySQL 5.5.54](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/json.php


### Data-Store/Database Mapping Test

Raw:
http://localhost:8080/dbraw.php

ORM:
http://localhost:8080/dborm.php

### Variable Query Test

Raw:
http://localhost:8080/dbraw.php?queries=5

ORM:
http://localhost:8080/dborm.php?queries=5
