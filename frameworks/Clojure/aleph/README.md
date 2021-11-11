# Compojure Benchmarking Test

This is the [Aleph](https://github.com/ztellman/aleph) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](hello/src/hello/handler.clj)

## Infrastructure Software Versions
The dependencies are documented in [project.clj](hello/project.clj),
but the main ones are:

* [Aleph 0.4.5-alpha6](https://github.com/ztellman/aleph)
* [Clojure 1.9.0](http://clojure.org/)
* [metosin/jsonista 0.2.0](https://github.com/metosin/jsonista), which in turn uses [Jackson](http://jackson.codehaus.org/)

## Test URLs
### JSON Encoding Test

http://localhost/json

### Plaintext Test

http://localhost/plaintext
