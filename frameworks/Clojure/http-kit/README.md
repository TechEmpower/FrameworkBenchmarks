# Compojure (using http-kit) Benchmarking Test

This is the Compojure (using http-kit) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](hello/src/hello/handler.clj)

### Data-Store/Database Mapping Test

* [Database test source](hello/src/hello/handler.clj)

## Infrastructure Software Versions
The dependencies are documented in [project.clj](hello/project.clj),
but the main ones are:

* [Clojure 1.8.0](http://clojure.org/)
* [http-kit](http://http-kit.org)
* [Ring-JSON 0.4.0](https://github.com/ring-clojure/ring-json), which in turn uses [Cheshire](https://github.com/dakrone/cheshire), which in turn uses [Jackson](http://jackson.codehaus.org/)
* [Korma 0.4.2](http://sqlkorma.com/)
