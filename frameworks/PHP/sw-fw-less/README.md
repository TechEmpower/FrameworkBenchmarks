# [Sw-Fw-Less](https://github.com/luoxiaojun1992/sw-fw-less) Benchmarking Test

This is the [Sw-Fw-Less](https://github.com/luoxiaojun1992/sw-fw-less) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](app/services/TestService.php)

### Plaintext Test

* [Plaintext test source](app/services/TestService.php)

### Data-Store/Database Mapping Test

* [DB test controller](app/services/TestService.php)
* [DB test model](app/models/World.php)

### Fortunes Test
* [Fortunes test controller](app/services/TestService.php)
* [Fortunes test model](app/models/Fortune.php)


## Infrastructure Software Versions
The tests were run with:
* [PHP 7.4](https://www.php.net/)
* [Swoole v4.5.6](https://www.swoole.com/)

## Test URLs
### JSON Encoding Test

http://localhost:9501/json

### Plaintext Test

http://localhost:9501/plaintext

### Data-Store/Database Mapping Test

http://localhost:9501/db

### Variable Query Test

http://localhost:9501/queries/2

### Data Updates Test

http://localhost:9501/updates/2

### Fortunes Test

http://localhost:9501/fortunes
