# Rack-app Benchmarking Test

rack-app is a minimalist web framework that focuses on simplicity and
maintainability. The framework is meant to be used by seasoned web developers.

https://github.com/rack-app/rack-app

### Test Type Implementation Source Code

* [JSON Serialization](app.rb): "/json"
* [Single Database Query](app.rb): "/db"
* [Multiple Database Queries](app.rb): "/db?queries={#}"
* [Fortunes](app.rb): "/fortune"
* [Plaintext](app.rb): "/plaintext"

## Important Libraries

The tests were run with:

* [Sequel](https://rubygems.org/gems/sequel)
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

### FORTUNES

http://localhost:8080/fortunes

