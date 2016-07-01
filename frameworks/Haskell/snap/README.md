# Snap Benchmarking Test

This is the Snap portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

* [Source](bench/src/Main.hs)
* [Configurations](bench/cfg/db.cfg)

## Infrastructure Software Versions
The tests were run with:
* GHC 7.10.3
* snap-core 0.9.8.0
* snap-server 0.9.5.1
* aeson 0.11.2.0
* configurator 0.3.0.0
* resource-pool 0.2.3.2
* mysql-simple 0.2.2.5

## Test URLs
### JSON Encoding Test

http://localhost:8000/json

### Data-Store/Database Mapping Test

http://localhost:8000/db

### Variable Query Test

http://localhost:8000/db?queries=2
