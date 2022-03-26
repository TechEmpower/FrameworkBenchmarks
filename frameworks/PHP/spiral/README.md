# Spiral Framework 2.0 Benchmarking Test
This is the Spiral Framework portion of a [benchmarking test suite](../) comparing a variety of web development platforms.
Tests are run with realistic approach, full ORM and templating stack.

Benchmark code is located in `app/src/Controllers/BenchmarkController.php`.

## Infrastructure and Versions
The tests were run with:
* [Spiral Framework Version 2](https://github.com/spiral/framework/)
* [Spiral/Stempler](https://github.com/spiral/stempler) as template engine
* [Cycle ORM 2.*](https://github.com/cycle/orm)
* [RoadRunner 2.*](https://roadrunner.dev/)
* [PHP Version 8.0.*](http://www.php.net/) in CLI mode with OPCache

## Test URLs
Test                | URL 
---                 | ---
JSON Encoding       | http://localhost:8080/json
Data-Store/Database | http://localhost:8080/db
Variable Query      | http://localhost:8080/db/:queries
Templating and ORM  | http://localhost:8080/fortunes
Update ORM          | http://localhost:8080/updates/:queries
Plain Text          | http://localhost:8080/plaintext
