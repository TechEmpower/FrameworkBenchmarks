# Padrino Framework

The information below contains information specific to Padrino. 
For further guidance, review the 
[documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/). 
Also note the additional information provided in the [Ruby README](../).

This is the Ruby Padrino portion of a [benchmarking test suite](../../) 
comparing a variety of web servers.

## Infrastructure Software Versions
The tests were run with:

* [Ruby 2.0.0-p0](http://www.ruby-lang.org/)
* [JRuby 1.7.8](http://jruby.org/)
* [Rubinius 2.2.10](http://rubini.us/)
* [Padrino 0.12.3](http://www.padrinorb.com/)
* [Rack 1.5.2](http://rack.github.com/)
* [Unicorn 4.8.3](http://unicorn.bogomips.org/)
* [TorqBox 0.1.7](http://torquebox.org/torqbox/)
* [Puma 2.9.0](http://puma.io/)
* [Thin 1.6.2](http://code.macournoyer.com/thin/)

## Paths & Source for Tests

* [JSON Serialization](app/controllers.rb): "/json"
* [Single Database Query](app/controllers.rb): "/db", [World Model](models/world.rb)
* [Multiple Database Queries](app/controllers.rb): "/query?queries={#}", [World Model](models/world.rb)
* [Fortunes](app/controllers.rb): "/fortunes", [Fortunes Model](models/fortune.rb)
* [Database Updates](app/controllers.rb): "/updates?queries={#}", [World Model](models/world.rb)
* [Plaintext](app/controllers.rb): "/plaintext"

## Get Help

### Experts

_No experts listed, yet. If you're an expert, add yourself!_

### [Community](http://www.padrinorb.com/pages/contribute)

* [Padrino Google Group](https://groups.google.com/forum/#!forum/padrino)
* `#padrino` IRC Channel([irc.freenode.net](http://freenode.net/))

### Resources

* [Padrino framework Source Code](https://github.com/padrino/padrino-framework)
