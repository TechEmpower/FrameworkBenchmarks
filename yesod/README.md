# Yesod Benchmarking Test

This is the Yesod portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

* [Controllers](bench/Application.hs)
* [Model](bench/config/models)

## Infrastructure Software Versions
The tests were run with:
* GHC 7.4.1
* Yesod 1.1.9.2

## Test URLs
### JSON Encoding Test

http://localhost:8000/json

### Data-Store/Database Mapping Test

http://localhost:8000/db

### Variable Query Test

http://localhost:8000/db2/2
