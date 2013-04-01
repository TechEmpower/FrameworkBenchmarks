# Django Benchmarking Test

This is the Django portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](hello/world/views.py)


### Data-Store/Database Mapping Test

* [DB test controller](hello/world/views.py)
* [DB test model](hello/world/models.py)


## Infrastructure Software Versions
The tests were run with:
* [Python 2.7.3](http://www.python.org/)
* [Django 1.4](https://www.djangoproject.com/)
* [Gunicorn 0.17.2](http://gunicorn.org/)
* [MySQL 5.5.29](https://dev.mysql.com/)


## Resources
* https://docs.djangoproject.com/en/dev/intro/tutorial01/

## Test URLs
### JSON Encoding Test

http://localhost:8080/json

### Data-Store/Database Mapping Test

http://localhost:8080/db

### Variable Query Test

http://localhost:8080/db?queries=2