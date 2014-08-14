# Ruby on Rails Benchmarking Test

This is the Ruby on Rails portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
* [JSON test source](app/controllers/hello_world_controller.rb)

### Data-Store/Database Mapping Test

* [Database test source](app/controllers/hello_world_controller.rb)


## Infrastructure Software Versions
The tests were run with:
* [Ruby 2.1.2](http://www.ruby-lang.org/)
* [JRuby 1.7.13](http://jruby.org/)
* [Rails 4.1.x](http://rubyonrails.org/)
* [Unicorn 4.8.3](http://unicorn.bogomips.org/)
* [TorqBox 0.1.7](http://torquebox.org/torqbox/)
* [MySQL 5.5](https://dev.mysql.com/)

## References
* https://github.com/FooBarWidget/passenger/pull/71

## Test URLs
### JSON Encoding Test

Ruby:
localhost:8080/hello_world/json

JRuby:
localhost:8080/hello_world/json

### Data-Store/Database Mapping Test

Ruby:
localhost:8080/hello_world/db?queries=5

JRuby:
localhost:8080/hello_world/db?queries=5
