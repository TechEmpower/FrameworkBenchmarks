# Luminus Benchmarking Test

This is the Luminus portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](hello/src/hello/handler.clj)

### Data-Store/Database Mapping Test

* [Database test source](hello/src/hello/handler.clj)

## Infrastructure Software Versions
The dependencies are documented in [project.clj](hello/project.clj),
but the main ones are:

* [Clojure 1.5.1](http://clojure.org/)
* [lib-noor 0.5.5](https://github.com/noir-clojure/lib-noir)
* [Compojure 1.1.5](https://github.com/weavejester/compojure)
* [Cheshire 5.1.1](https://github.com/dakrone/cheshire), which in turn uses [Jackson](http://jackson.codehaus.org/)
* [Korma 0.3.0-RC5](http://sqlkorma.com/)

## Test URLs
### JSON Encoding Test

http://localhost/hello-compojure-standalone/json

### Data-Store/Database Mapping Test

http://localhost/hello-compojure-standalone/db

### Variable Query Test

http://localhost/hello-compojure-standalone/db/2
