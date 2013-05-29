# Ruby on Rails Benchmarking Test

This is the Ruby on Rails portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
* [JSON test source](app/controllers/hello_world_controller.rb)

### Data-Store/Database Mapping Test

* [Database test source](app/controllers/hello_world_controller.rb)


## Infrastructure Software Versions
The tests were run with:
* [Ruby 2.0.0-p0](http://www.ruby-lang.org/)
* [JRuby 1.7.4](http://jruby.org/)
* [Rails 3.2.11](http://rubyonrails.org/)
* [Unicorn 4.6.2](http://unicorn.bogomips.org/)
* [Resin 4.0.34](http://www.caucho.com/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## References
* https://github.com/FooBarWidget/passenger/pull/71
* https://github.com/jruby/jruby-rack/issues/146

## Test URLs
### JSON Encoding Test

Ruby:
localhost:8080/hello_world/json

JRuby:
localhost:8080/rails/hello_world/json

### Data-Store/Database Mapping Test

Ruby:
localhost:8080/hello_world/db?queries=5

JRuby:
localhost:8080/rails/hello_world/db?queries=5
