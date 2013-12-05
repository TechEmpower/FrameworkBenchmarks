# Ruby Sinatra Benchmarking Test

This is the Ruby Sinatra portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
* [JSON test source](hello_world.rb)

### Data-Store/Database Mapping Test

* [Database test source](hello_world.rb)

## Infrastructure Software Versions
The tests were run with:
* [Ruby 2.0.0-p0](http://www.ruby-lang.org/)
* [JRuby 1.7.8](http://jruby.org/)
* [Sinatra 1.3.4](http://www.sinatrarb.com/)
* [Unicorn 4.6.2](http://unicorn.bogomips.org/)
* [TorqBox 0.1.4](http://torquebox.org/torqbox/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## References
* https://github.com/FooBarWidget/passenger/pull/71

## Test URLs

### JSON Encoding Test

Ruby:
localhost:8080/json

JRuby:
localhost:8080/json

### Data-Store/Database Mapping Test

Ruby:
localhost:8080/db?queries=5

JRuby:
localhost:8080/db?queries=5
