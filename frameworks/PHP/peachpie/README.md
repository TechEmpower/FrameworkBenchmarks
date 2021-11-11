# Peachpie Benchmarking Test

This is the [Peachpie](http://github.com/peachpiecompiler/peachpie) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

As we want Peachpie to be comparable to the PHP Interpreter, we derived our tests from theirs with the following modifications:

1. Some logic was updated to prevent warnings during verification.

2. Global code was enclosed in functions. Not only is Peachpie able to optimize such code better, but it is also a common programming practice.

3. Because PDO is not yet implemented in Peachpie, old-fashioned MySQL functions such as `mysql_connect()` are used. We are going to replace them by PDO eventually.

4. Currently, only raw database tests without ORM are performed.

## Test Sources

* [JSON Serialization](./Website/json.php)
* [Database Queries](./Website/dbraw.php)
* [Fortunes](./Website/fortune.php)
* [Database Updates](./Website/updateraw.php)
* [Plaintext](./Website/plaintext.php)

## Test URLs

* http://localhost:8080/json.php
* http://localhost:8080/dbraw.php
* http://localhost:8080/dbraw.php?queries=10
* http://localhost:8080/fortunes.php
* http://localhost:8080/updateraw.php
* http://localhost:8080/updateraw.php?queries=10
* http://localhost:8080/plaintext.php

## Contacts

* [Jakub Míšek](http://github.com/jakubmisek) - Chief developer of Peachpie
* [Robert Husák](http://github.com/roberthusak) - Author of this contribution
