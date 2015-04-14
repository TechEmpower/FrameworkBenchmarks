# Grape Micro-Framework

The information below contains information specific to Grape. 
For further guidance, review the 
[documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/). 
Also note the additional information provided in the [Ruby README](../).

This is the Ruby Grape portion of a [benchmarking test suite](../../) 
comparing a variety of web servers along with JRuby/MRI.

## Infrastructure Software Versions
The tests were run with:

* [Ruby 2.0.0-p0](http://www.ruby-lang.org/)
* [JRuby 1.7.8](http://jruby.org/)
* [Rubinius 2.2.10](http://rubini.us/)
* [Grape 0.8.0](http://intridea.github.io/grape/)
* [Rack 1.5.2](http://rack.github.com/)
* [Unicorn 4.8.3](http://unicorn.bogomips.org/)
* [TorqBox 0.1.7](http://torquebox.org/torqbox/)
* [Puma 2.9.0](http://puma.io/)
* [Thin 1.6.2](http://code.macournoyer.com/thin/)

## Paths & Source for Tests

* [JSON Serialization](config.ru): "/json"
* [Single Database Query](config.ru): "/db"
* [Multiple Database Queries](config.ru): "/query?queries={#}"
* _Fortunes: N/A_
* [Database Updates](config.ru): "/updates?queries={#}"
* [Plaintext](config.ru): "/plaintext"

## Get Help

### Experts

_No experts listed, yet. If you're an expert, add yourself!_

### Community

* [Grape Google Group](https://groups.google.com/forum/?fromgroups#!forum/ruby-grape)

### Resources

* [Grape Micro-framework Source Code](https://github.com/intridea/grape)
