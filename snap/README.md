# Snap Benchmarking Test

This is the Snap portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

* [Source](bench/src/Main.hs)
* [Configurations](bench/cfg/db.cfg)

## Infrastructure Software Versions
The tests were run with:
* GHC 7.4.1
* snap-core 0.9.3.1
* snap-server 0.9.3.3
* json 0.7
* configurator 0.2.0.2
* resource-pool 0.2.1.1
* HDBC-mysql 0.6.6.1
* HDBC 2.3.1.2

## Test URLs
### JSON Encoding Test

http://localhost:8000/json

### Data-Store/Database Mapping Test

http://localhost:8000/db

### Variable Query Test

http://localhost:8000/db?queries=2
