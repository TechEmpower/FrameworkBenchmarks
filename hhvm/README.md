#HHVM Benchmarking Test

This is the [HHVM](http://github.com/facebook/hhvm) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### Plaintext Test

* [Plaintext test source](plaintext.php)

### Fortune Test

* [Fortune test source](fortune.php)


### JSON Encoding Test
Use the built-in JSON Encoder

* [JSON test source](json.php)

### Data-Store/Database Mapping Test

* [Database test source Raw](dbraw.php)
* [Database test source ORM](dborm.php)

## Infrastructure Software Versions
The tests were run with:

* [HHVM v2.2.0](http://github.com/facebook/hhvm)

## Test URLs
### JSON Encoding Test

http://localhost/json.php

### Data-Store/Database Mapping Test

Raw:
http://localhost/dbraw.php

ORM:
http://localhost/dborm.php

### Variable Query Test

Raw:
http://localhost/dbraw.php?queries=5

ORM:
http://localhost/dborm.php?queries=5
