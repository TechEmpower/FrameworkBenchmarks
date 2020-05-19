# Reitit Benchmarking Test

This is the [Reitit](https://github.com/metosin/reitit) portion of a [benchmarking test suite](../) comparing a variety of web development platforms, both witb JDBC via [HikariCP](https://github.com/tomekw/hikari-cp) and [The Reactive SQL Client](https://github.com/eclipse-vertx/vertx-sql-client).

### Source

* [`handler.clj`](hello/src/hello/handler.clj)

## Infrastructure Software Versions

The dependencies are documented in [project.clj](hello/project.clj),
but the main ones are:

* [org.clojure/clojure](http://clojure.org/) - latest Clojure version
* [metosin/pohjavirta](https://github.com/metosin/pohjavirta) - fast wrapper for [Undertow](http://undertow.io/)
* [metosin/jsonista](https://github.com/metosin/jsonista) - fast JSON marchalling for Clojure
* [metosin/reitit](https://github.com/metosin/reitit) - fast routing lib for Clojure/Script
* [metosin/porsas](https://github.com/metosin/porsas) - fast database access for Clojure

## Test URLs

* http://localhost:8080/json
* http://localhost:8080/plaintext
* http://localhost:8080/db
