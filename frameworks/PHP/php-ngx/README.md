# Ngx PHP Benchmarking Test

This is the PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

[ngx_php7](https://github.com/rryqszq4/ngx_php7) is an extension module of high-performance web server nginx, which implements embedded php7 script to process nginx location and variables.

ngx_php7 draws on the design of ngx_lua and is committed to providing non-blocking web services with significant performance advantages over php-cgi, mod_php, php-fpm and hhvm.



## Infrastructure Software Versions
The tests were run with [PHP](https://www.php.net/) with OPcache and JIT

### Platform

* [ngx-php](https://github.com/rryqszq4/ngx_php7) (Embeded php in nginx)

### Webservers

* [nginx](https://nginx.org/)


### Databases

* [MySQL](https://dev.mysql.com/)
* [PostgreSQL](https://www.postgresql.org/)


## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

Raw:
http://localhost/db

### Variable Query Test

Raw:
http://localhost/query?queries=5
