# weppy Benchmark Test (ported from Flask example)

This is the weppy portion of a [benchmarking tests suite](../../) 
comparing a variety of web development platforms.

The information below is specific to weppy. For further guidance, 
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki). 
Also note that there is additional information provided in 
the [Python README](../).

[weppy](http://weppy.org/) is a fullstack Python 2/3 web framework.
This test uses the included ORM and templating system, and gunicorn+meinheld for the application server on CPtyhon and Tornado on pypy.

## Test Paths & Source

* [JSON Serialization](app.py): "/json"
* [Single Database Query](app.py): "/db"
* [Multiple Database Queries](app.py): "queries?queries=#"
* [Fortunes](app.py): "/fortunes"
* [Database Updates](app.py): "updates?queries=#"
* [Plaintext](app.py): "/plaintext"

*Replace # with an actual number.

### Resources

* [weppy Source Code](https://github.com/gi0baro/weppy)
* [weppy Documentation](http://weppy.org/docs)

### Community

* [weppy (weppy-talk) Google Group](https://groups.google.com/forum/#!forum/weppy-talk)

