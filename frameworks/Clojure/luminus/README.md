# Luminus Benchmarking Test

This is the [Luminus](http://www.luminusweb.net/) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](hello/src/hello/handler.clj)

### Data-Store/Database Mapping Test

* [Database test source](hello/src/hello/handler.clj)

## Infrastructure Software Versions
The dependencies are documented in [project.clj](hello/project.clj),
but the main ones are:

* [Clojure 1.8.0](http://clojure.org/)
* [Compojure 1.4.0](https://github.com/weavejester/compojure)
* [Cheshire 5.5.0](https://github.com/dakrone/cheshire), which in turn uses [Jackson](http://jackson.codehaus.org/)

