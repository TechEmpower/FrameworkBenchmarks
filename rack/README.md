# Ruby Rack Benchmarking Test

This is the Ruby Rack portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test
* [JSON test source](config.ru)


## Infrastructure Software Versions
The tests were run with:

* [Ruby 2.0.0-p0](http://www.ruby-lang.org/)
* [JRuby 1.7.4](http://jruby.org/)
* [Rack 1.5.1](http://rack.github.com/)
* [Unicorn 4.6.2](http://unicorn.bogomips.org/)
* [Resin 4.0.34](http://www.caucho.com/)

## References
* https://github.com/FooBarWidget/passenger/pull/71
* https://github.com/jruby/jruby-rack/issues/146

## Test URLs
### JSON Encoding Test

Ruby:
localhost:8080

JRuby:
localhost:8080/rack/

