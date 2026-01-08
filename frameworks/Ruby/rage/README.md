# Rage Benchmarking Test

Rage is a fast web framework compatible with Rails. It uses an event-driven architecture and implements a lightweight, cooperative concurrency model based on Ruby Fibers.

https://github.com/rage-rb/rage

### Test Type Implementation Source Code

* [JSON](app/controllers/benchmarks_controller.rb)
* [PLAINTEXT](app/controllers/benchmarks_controller.rb)
* [DB](app/controllers/benchmarks_controller.rb)
* [QUERY](app/controllers/benchmarks_controller.rb)
* [UPDATE](app/controllers/benchmarks_controller.rb)
* [FORTUNES](app/controllers/benchmarks_controller.rb)

## Important Libraries

The tests were run with:

* [ActiveRecord](https://rubygems.org/gems/activerecord)
* [PG](https://rubygems.org/gems/pg)

## Test URLs

### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/queries?queries=

### UPDATE

http://localhost:8080/updates?queries=

### FORTUNES

http://localhost:8080/fortunes
