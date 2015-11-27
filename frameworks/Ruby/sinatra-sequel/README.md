# Ruby [Sinatra](http://www.sinatrarb.com/) Benchmarking Test

The information below contains information specific to Sinatra. 
For further guidance, review the 
[documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/). 
Also note the additional information provided in the [Ruby README](../).

This is the Ruby Sinatra portion of a [benchmarking test suite](../../) 
comparing a variety of web platforms.

## Infrastructure Software Versions
The tests were run with:
* [Ruby 2.0.0-p0](http://www.ruby-lang.org/)
* [JRuby 1.7.8](http://jruby.org/)
* [Rubinius 2.2.10](http://rubini.us/)
* [Sinatra 1.3.4](http://www.sinatrarb.com/)
* [Unicorn 4.6.2](http://unicorn.bogomips.org/)
* [TorqBox 0.1.7](http://torquebox.org/torqbox/)
* [Puma 2.9.0](http://puma.io/)
* [Thin 1.6.2](http://code.macournoyer.com/thin/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Paths & Source for Tests

* [JSON Serialization](hello_world.rb): "/json"
* [Single Database Query](hello_world.rb): "/db"
* [Multiple Database Queries](hello_world.rb): "/db?queries={#}"
* [Fortunes](hello_world.rb): "/fortune"
* [Database Updates](hello_world.rb): "/update?queries={#}"
* [Plaintext](hello_world.rb): "/plaintext"

## Get Help

### Experts

_No experts listed, yet. If you're an expert, add yourself!_

### Community

* [Sinatra Google Group](https://groups.google.com/forum/#!forum/sinatrarb)
* `#sinatra` IRC Channel ([irc.freenode.net](http://freenode.net/))

### Resources

* [Sinatra Source Code](https://github.com/sinatra/sinatra)
* [PR: passenger-install-apache2-module doesn't work on ruby 2.0](https://github.com/FooBarWidget/passenger/pull/71)
