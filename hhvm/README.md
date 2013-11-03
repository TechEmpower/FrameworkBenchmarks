#HHVM Benchmarking Test

This is the [HHVM](http://github.com/facebook/hhvm) portion of a [benchmarking test suite](../)

Supports the Following Benmarking URLs

* http://localhost:8080/json
* http://localhost:8080/db
* http://localhost:8080/db?queries=10
* http://localhost:8080/queries
* http://localhost:8080/queries?queries=10
* http://localhost:8080/fortunes
* http://localhost:8080/updates
* http://localhost:8080/updates?queries=10
* http://localhost:8080/plaintext

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
