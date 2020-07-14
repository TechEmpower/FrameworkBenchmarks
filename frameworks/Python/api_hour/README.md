# AsyncIO/aiohttp/API-Hour Benchmark Test

This is the AsyncIO/aiohttp/API-Hour portion of a [benchmarking tests suite](../../) 
comparing a variety of web development platforms.

The information below is specific to AsyncIO/API-Hour. For further guidance, 
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki). 
Also note that there is additional information provided in 
the [Python README](../).

## Test Paths & Sources

* [JSON Serialization](aiohttp.web/hello/endpoints/world.py): "/json"
* [Single Database Query](aiohttp.web/hello/services/world.py): "/db"
* [Multiple Database Queries](aiohttp.web/hello/services/world.py): "/queries?queries=#"*
* [Fortunes](aiohttp.web/hello/services/world.py): "/fortunes"
* [Database Updates](aiohttp.web/hello/services/world.py): "/updates?queries=#"*
* [Plaintext](aiohttp.web/hello/endpoints/world.py): "/plaintext"

*Replace # with an actual number.

## Get Help

### Community

* [API-Hour Google Group](https://groups.google.com/forum/#!forum/api-hour)

### Resources

* [API-Hour Source Code](https://github.com/Eyepea/API-Hour)
