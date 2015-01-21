#HHVM Benchmarking Test

This is the [HHVM](http://github.com/facebook/hhvm) portion of a [benchmarking test suite](../)

Supports the Following Benmarking URLs

* http://localhost:8080/json.php
* http://localhost:8080/db.php
* http://localhost:8080/db.php?queries=10
* http://localhost:8080/queries.php
* http://localhost:8080/queries.php?queries=10
* http://localhost:8080/fortunes.php
* http://localhost:8080/updates.php
* http://localhost:8080/updates.php?queries=10
* http://localhost:8080/plaintext.php

### 1. Plaintext Test

* [Plaintext - source](plaintext.php)

### 2. Single database query

* [Single database query - source](db.php)

### 3. Multiple database queries

* [Multiple database queries - source](queries.php)

### 4. Fortune test

* [Fortunes - source](fortunes.php)

### 5. Database updates test

* [Database updates - source](updates.php)

### 6. Plaintext test

* [Plaintext - source](plaintext.php)

## Infrastructure Software Versions
The tests were run with:

* [HHVM v2.2.0](http://github.com/facebook/hhvm)

## Setting up a test environment

1. Invoke the ./setup.sh script

2. Invoke the ./run.sh script in another terminal

3. Invoke the ./tests.sh script and see that you get a sample output from all
    the urls mentions in the ./URLs file
