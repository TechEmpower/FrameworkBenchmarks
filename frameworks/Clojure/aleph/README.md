# Compojure Benchmarking Test

This is the [Aleph](https://github.com/clj-commons/aleph) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](src/hello/handler.clj)

## Infrastructure Software Versions
The dependencies are documented in [project.clj](project.clj),
but the main ones are:

* [Aleph 0.4.7](https://github.com/clj-commons/aleph)
* [Clojure 1.11.0](http://clojure.org/)
* [metosin/jsonista 0.3.5](https://github.com/metosin/jsonista), which in turn uses [Jackson](http://jackson.codehaus.org/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Plaintext Test

http://localhost/plaintext
