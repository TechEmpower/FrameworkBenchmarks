# [Ruby on Rails](http://rubyonrails.org/) Benchmarking Test

The information below contains information specific to Ruby on Rails. 
For further guidance, review the 
[documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/). 
Also note the additional information provided in the [Ruby README](../).

This is the Ruby on Rails portion of a [benchmarking test suite](../../) 
comparing a variety of web platforms.

## Infrastructure Software Versions

The tests were run with:

* [Ruby 2.0.0-p0](http://www.ruby-lang.org/)
* [JRuby 1.7.8](http://jruby.org/)
* [Rails 3.2.18](http://rubyonrails.org/)
* [Unicorn 4.8.3](http://unicorn.bogomips.org/)
* [TorqBox 0.1.7](http://torquebox.org/torqbox/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Paths & Source for Tests

* [JSON Serialization](app/controllers/hello_world_controller.rb): "/json"
* [Single Database Query](app/controllers/hello_world_controller.rb): "/db", [World Model](app/models/world.rb)
* [Multiple Database Queries](app/controllers/hello_world_controller.rb): "/db?queries={#}", [World Model](app/models/world.rb)
* _Fortunes: N/A_
* _Database Updates: N/A_
* _Plaintext: N/A_

## Get Help

### Experts

_No experts listed, yet. If you're an expert, add yourself!_

### Community

* `#rubyonrails` IRC Channel ([irc.freenode.net](http://freenode.net/))
* [Ruby on Rails Twitter](https://twitter.com/rails)
* [Ruby on Rails Google Group](https://groups.google.com/forum/#!forum/rubyonrails-talk)

### Resources

* [Ruby on Rails Source Code](https://github.com/rails/rails)
* [PR: passenger-install-apache2-module doesn't work on ruby 2.0](https://github.com/FooBarWidget/passenger/pull/71)
