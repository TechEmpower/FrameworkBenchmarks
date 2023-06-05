# Grape Micro-Framework

The information below contains information specific to Grape.
For further guidance, review the
[documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note the additional information provided in the [Ruby README](../).

This is the Ruby Grape portion of a [benchmarking test suite](../../)
comparing a variety of web servers.

## Infrastructure Software Versions
The tests were run with:

* [Ruby 3.1](http://www.ruby-lang.org/)
* [Grape 1.6.2](http://www.ruby-grape.org/)
* [Rack 2.2.3.1](https://rack.github.io/)
* [Unicorn 6.1.0](https://yhbt.net/unicorn/)
* [Puma 5.6.4](https://puma.io/)

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

* [Grape Micro-framework Source Code](https://github.com/ruby-grape/grape)
