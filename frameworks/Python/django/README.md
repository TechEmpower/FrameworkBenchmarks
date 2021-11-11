# [Django](https://www.djangoproject.com/) Benchmarking Test

This is the Django portion of a [benchmarking tests suite](../../)
comparing a variety of web development platforms.

The information below is specific to Django. For further guidance,
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note that there is additional information provided in
the [Python README](../).

## Test Paths & Sources

* [JSON Serialization](hello/world/views.py): "/json"
* [Single Database Query](hello/world/views.py): "/db", [World Model](hello/world/models.py)
* [Multiple Database Queries](hello/world/views.py): "/dbs?queries=#"*, [World Model](hello/world/models.py)
* [Fortunes](hello/world/views.py): "/fortunes", [Fortune Model](hello/world/models.py)
* [Database Updates](hello/world/views.py): "/update?queries=#"*, [World Model](hello/world/models.py)
* [Plaintext](hello/world/views.py): "/plaintext" 

*Replace # with an actual number.

## Get Help

### [Community](https://www.djangoproject.com/community/)

* `#django` IRC Channel ([irc.freenode.net](https://freenode.net/))

### Resources

* [Writing your first Django app](https://docs.djangoproject.com/en/dev/intro/tutorial01/)
