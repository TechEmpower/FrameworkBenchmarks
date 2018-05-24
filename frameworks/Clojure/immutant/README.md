# Immutant Benchmarking Test

This is the [Immutant](http://immutant.org/) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](hello/src/hello/handler.clj)

## Infrastructure Software Versions

The dependencies are documented in [project.clj](hello/project.clj),
but the main ones are:

* [Clojure 1.9.0](http://clojure.org/)
* [ikitommi/immutant-web "3.0.0-alpha1"](https://github.com/ikitommi/immutant) - a performant fork of [Immutant]((http://immutant.org/))
* [metosin/jsonista "0.2.0"](https://github.com/metosin/jsonista)

## Test URLs
### JSON Encoding Test

http://localhost:8080/json

### Plaintext Test

http://localhost:8080/plaintext
