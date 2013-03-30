# Yesod Benchmarking Test

This is the Yesod portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](hello/world/views.py)


### Data-Store/Database Mapping Test

* [DB test controller](hello/world/views.py)
* [DB test model](hello/world/models.py)


## Infrastructure Software Versions
The tests were run with:
* GHC 7.4.1
* Yesod

## Test URLs
### JSON Encoding Test

http://localhost:3000/json

### Data-Store/Database Mapping Test

http://localhost:3000/db

### Variable Query Test

http://localhost:3000/db2/2