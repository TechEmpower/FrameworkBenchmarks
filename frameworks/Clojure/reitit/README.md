# Reitit Benchmarking Test

This is the [Reitit](https://github.com/metosin/reitit) portion of a [benchmarking test suite](../) comparing a variety of web development platforms, both witb blocking JDBC via [HikariCP](https://github.com/tomekw/hikari-cp) and [The Reactive SQL Client](https://github.com/eclipse-vertx/vertx-sql-client).

### Source

* [`handler.clj`](hello/src/hello/handler.clj)

## Infrastructure Software Versions

The dependencies are documented in [project.clj](hello/project.clj),
but the main ones are:

* [Clojure 1.10.0](http://clojure.org/)
* [ikitommi/immutant-web "3.0.0-alpha4"](https://github.com/ikitommi/immutant) - a performant fork of [Immutant]((http://immutant.org/))
* [metosin/jsonista "0.2.2"](https://github.com/metosin/jsonista) - fast JSON marchalling for Clojure
* [metosin/reitit "0.3.7"](https://github.com/metosin/reitit) - fast routing lib for Clojure/Script
* [metosin/porsas "0.0.1-alpha7"](https://github.com/metosin/porsas) - fast database access for Clojure

## Test URLs

* http://localhost:8080/json
* http://localhost:8080/plaintext
* http://localhost:8080/db
