# Compojure (using http-kit) Benchmarking Test

This is the Compojure (using http-kit) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](hello/src/hello/handler.clj)

### Data-Store/Database Mapping Test

* [Database test source](hello/src/hello/handler.clj)

## Infrastructure Software Versions
The dependencies are documented in [project.clj](hello/project.clj),
but the main ones are:

* [Clojure 1.5.1](http://clojure.org/)
* [Compojure 1.1.5](https://github.com/weavejester/compojure)
* [Ring-JSON 0.2.0](https://github.com/ring-clojure/ring-json), which in turn uses [Cheshire](https://github.com/dakrone/cheshire), which in turn uses [Jackson](http://jackson.codehaus.org/)
* [http-kit](http://http-kit.org)
* [dbcp.clj](https://github.com/http-kit/dbcp.clj)

## Test URLs
### JSON Encoding Test

http://localhost/http-kit/json

### Data-Store/Database Mapping Test

http://localhost/http-kit/db

### Variable Query Test

http://localhost/http-kit/db/2
