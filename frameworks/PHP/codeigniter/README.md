# Codeigniter PHP Benchmarking Test

This is the Codeigniter PHP portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
Uses the PHP standard [JSON encoder](http://www.php.net/manual/en/function.json-encode.php).

* [JSON test controller](app/Controllers/Bench.php)


### Data-Store/Database Mapping Test
Uses the db abstraction class from Codeigniter

* [DB test controller](app/Controllers/Bench.php)


## Infrastructure Software Versions
The tests were run with:

* [Codeigniter Version 4](https://www.codeigniter.com)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Data-Store/Database Mapping Test

http://localhost/db

### Variable Query Test
    
http://localhost/queries/2
