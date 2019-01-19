#PHP Benchmarking Test

This is the PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.
NGINX is removed in this test, and substituted by [Workerman, An asynchronous event driven PHP framework](https://github.com/walkor/Workerman). An asynchronous event driven PHP framework for easily building fast, scalable network applications. Supports HTTP, Websocket, SSL and other custom protocols. Supports libevent, HHVM, ReactPHP.

https://github.com/walkor/Workerman

This test doesn't use the standard PHP (fw_require) because it needs PCNTL / Process Control extension. Adding PCNTL in the PHP compilation (--enable-pcntl) will fail other PHP framework test.

```
Database config
HOST: DBHOST (from ENV) , or 127.0.0.1 if DBHOST is not available
User : benchmarkdbuser
Password : benchmarkdbpass
DBNAME : hello_world
```
MySQL Connection is using PHP PDO::Persistent Connection.

The number of threads count in PHP is (number of cores)*2.

## Infrastructure Software Versions
The tests were run with:
* [PHP 7](http://www.php.net/)
* [MySQL 5.5.54](https://dev.mysql.com/)

Pre-test:
* [Composer](https://getcomposer.org/)

### JSON Encoding Test
Using the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php)

## Test URLs

### JSON Encoding Test
http://localhost:8080/json.php

### Data-Store/Database Mapping Test
http://localhost:8080/dbraw.php

http://localhost:8080/updateraw.php

### Variable Query Test
http://localhost:8080/dbraw.php?queries=5

### Fortune Test
http://localhost:8080/fortune.php
