# [Bottle](http://bottlepy.org/docs/dev/index.html) Benchmark Test

This is the Python Bottle portion of a [benchmarking tests suite](../../) 
comparing a variety of frameworks.

The information below is specific to Bottle. For further guidance, 
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki). 
Also note that there is additional information that's provided in 
the [Python README](../).

## Test Paths and Sources

All tests are implemented in a single file ([app.py](app.py)).

With ORM:

* [JSON Serialization](app.py): "/json"
* [Single Database Query](app.py): "/dbs"
* [Multiple Database Queries](app.py): "/db?queries=#"*
* [Fortunes](app.py): "/fortunes"
* [Database Updates](app.py): "/updates?queries=#"*
* [Plaintext](app.py): "/plaintext"

Without ORM (raw):

* [Single Database Query](app.py): "/raw-db"
* [Multiple Database Queries](app.py): "/raw-queries?=#"*
* [Fortune](app.py): "/raw-fortune"
* [Database Updates](app.py): "/raw-updates?queries=#"*

*Replace # with an actual number.

## Get Help

### Community

* [bottlepy Google Group](https://groups.google.com/forum/#!forum/bottlepy)
* `#bottlepy` IRC Channel ([irc.freenode.net](https://freenode.net/))
