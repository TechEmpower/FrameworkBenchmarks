#PHP Benchmarking Test

This is the PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.
NGINX is removed in this test, and substituted by [Workerman, An asynchronous event driven PHP framework](https://github.com/walkor/Workerman). An asynchronous event driven PHP framework for easily building fast, scalable network applications. Supports HTTP, Websocket, SSL and other custom protocols. Supports libevent, HHVM, ReactPHP.

https://github.com/walkor/Workerman

MySQL Connection is using PHP PDO::Persistent Connection.


## Infrastructure Software Versions
The tests were run with:
* [PHP 8](http://www.php.net/)


Pre-test:
* [Composer](https://getcomposer.org/)

### JSON Encoding Test
Using the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php)

## Test URLs

### JSON Encoding Test
http://localhost:8080/json

### Data-Store/Database Mapping Test
http://localhost:8080/db

http://localhost:8080/update

### Variable Query Test
http://localhost:8080/query

### Fortune Test
http://localhost:8080/fortunes
