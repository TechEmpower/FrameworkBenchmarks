# [Ruby on Rails](http://rubyonrails.org/) Benchmarking Test

The information below contains information specific to Ruby on Rails.
For further guidance, review the
[documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note the additional information provided in the [Ruby README](../).

This is the Ruby on Rails portion of a [benchmarking test suite](../../)
comparing a variety of web platforms.

## Infrastructure Software Versions

The tests were run with:

- [Ruby 2.6.0](http://www.ruby-lang.org/)
- [Rails 5.2.2](http://rubyonrails.org/)
- [Unicorn 5.4.1](http://unicorn.bogomips.org/)
- [Puma 3.12](http://puma.io/)
- [MySQL 5.5](https://dev.mysql.com/)
- [PostgreSQL 11](https://www.postgresql.org/)

## Paths & Source for Tests

- [JSON Serialization](app/controllers/hello_world_controller.rb): "/json"
- [Single Database Query](app/controllers/hello_world_controller.rb): "/db", [World Model](app/models/world.rb)
- [Multiple Database Queries](app/controllers/hello_world_controller.rb): "/db?queries={#}", [World Model](app/models/world.rb)
- [Fortunes](app/controllers/hello_world_controller.rb): "/fortune" , [Fortunes Model](app/models/fortune.rb)
- [Database Updates](app/controllers/hello_world_controller.rb): "/update?queries={#}", [World Model](app/models/world.rb)
- [Plaintext](app/controllers/hello_world_controller.rb): "/plaintext"

## Get Help

### Experts

_No experts listed, yet. If you're an expert, add yourself!_

### Community

- `#rubyonrails` IRC Channel ([irc.freenode.net](http://freenode.net/))
- [Ruby on Rails Twitter](https://twitter.com/rails)
- [Ruby on Rails Google Group](https://groups.google.com/forum/#!forum/rubyonrails-talk)

### Resources

- [Ruby on Rails Source Code](https://github.com/rails/rails)
- [PR: passenger-install-apache2-module doesn't work on ruby 2.0](https://github.com/FooBarWidget/passenger/pull/71)
