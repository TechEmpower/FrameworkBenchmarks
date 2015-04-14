# Ruby [Rack](http://rack.github.io/) Benchmarking Test

The information below contains information specific to Rack. 
For further guidance, review the 
[documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/). 
Also note the additional information provided in the [Ruby README](../).

This is the Ruby Rack portion of a [benchmarking test suite](../../) 
comparing a variety of web servers.

## Infrastructure Software Versions
The tests were run with:

* [Ruby 2.0.0-p0](http://www.ruby-lang.org/)
* [JRuby 1.7.8](http://jruby.org/)
* [Rubinius 2.2.10](http://rubini.us/)
* [Rack 1.5.2](http://rack.github.com/)
* [Unicorn 4.8.3](http://unicorn.bogomips.org/)
* [TorqBox 0.1.7](http://torquebox.org/torqbox/)
* [Puma 2.9.0](http://puma.io/)
* [Thin 1.6.2](http://code.macournoyer.com/thin/)

## Paths & Source for Tests

* [JSON Serialization](app/ruby_impl.rb) [[jruby version](app/jruby_impl.rb)]: "/json"
* [Single Database Query](app/ruby_impl.rb) [[jruby version](app/jruby_impl.rb)]: "/db", [World Model](models/world.rb)
* [Multiple Database Queries](app/ruby_impl.rb) [[jruby version](app/jruby_impl.rb)]: "/query?queries={#}", [World Model](models/world.rb)
* _Fortunes: N/A_
* [Database Updates](app/ruby_impl.rb) [[jruby version](app/jruby_impl.rb)]: "/updates?queries={#}", [World Model](models/world.rb)
* [Plaintext](app/ruby_impl.rb) [[jruby version](app/jruby_impl.rb)]: "/plaintext"

## Get Help

### Experts

_No experts listed, yet. If you're an expert, add yourself!_

### Community

* `#rack` IRC Channel ([irc.freenode.net](http://freenode.net/))
* [Rack Google Group](https://groups.google.com/forum/#!forum/rack-devel)

### Resources

* [Rack Source Code](https://github.com/rack/rack)
* [PR: passenger-install-apache2-module doesn't work on ruby 2.0](https://github.com/FooBarWidget/passenger/pull/71)
