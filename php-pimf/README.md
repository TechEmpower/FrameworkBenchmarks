# PIMF Benchmarking Test

This is the PIMF portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](/app/Vanilla/Controller/Hello.php)


### Data-Store/Database Mapping Test
Uses the PIMF PDO Entity-Manager.

* [DB test controller](/app/Vanilla/Controller/Hello.php)

### Template Test
Uses PIMF plain vanilla PHTML rendering

* [Template test controller](/app/Vanilla/Controller/Hello.php)


## Infrastructure Software Versions
The tests were run with:

* [PIMF Version 1.8.6](http://pimf-framework.de/)
* [PHP Version 5.4.13](http://www.php.net/) with FPM and APC
* [nginx 1.4.0](http://nginx.org/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db

### Variable Query Test
    
http://localhost/queries?queries=10

### Templating Test

http://localhost/fortunes

### Database Updates Test

http://localhost/updates?queries=10

### Plaintext Test

http://localhost/plaintext
