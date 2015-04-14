# API-Hour Benchmark Test

This is the API-Hour portion of a [benchmarking tests suite](../../) 
comparing a variety of web development platforms.

The information below is specific to API-Hour. For further guidance, 
review the [documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/). 
Also note that there is additional information provided in 
the [Python README](../).

## Test Paths & Sources

* [JSON Serialization](hello/endpoints/world.py): "/json"
* [Single Database Query](hello/services/world.py): "/db"
* [Multiple Database Queries](hello/services/world.py): "/queries?queries=#"*
* [Fortunes](hello/services/world.py): "/fortunes"
* [Database Updates](hello/services/world.py): "/updates?queries=#"*
* [Plaintext](hello/endpoints/world.py): "/plaintext"

*Replace # with an actual number.

## Get Help

### Community

* [API-Hour Google Group](https://groups.google.com/forum/#!forum/api-hour)

### Resources

* [API-Hour Source Code](https://github.com/Eyepea/API-Hour)
