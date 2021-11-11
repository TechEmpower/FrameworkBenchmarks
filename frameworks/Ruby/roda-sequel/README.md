# Ruby [Roda](http://roda.jeremyevans.net)-[Sequel](http://sequel.jeremyevans.net) Benchmarking Test

The information below contains information specific to the Roda benchmarking
test. For further guidance, review the
[documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki). Also
note the additional information provided in the [Ruby README](../).

This is the Ruby Roda portion of a [benchmarking test suite](../../)
comparing a variety of web platforms.

## Infrastructure Software Versions

The tests will be run with:

* [Ruby 2.4](http://www.ruby-lang.org)
* [JRuby 9.1](http://jruby.org)
* [Rubinius 3](https://rubinius.com)\*
* [Puma 3](http://puma.io)
* [Passenger 5](https://www.phusionpassenger.com)
* [Unicorn 5](https://bogomips.org/unicorn/)
* [TorqueBox 4.0](http://torquebox.org)
* [Roda 3](http://roda.jeremyevans.net)
* [Sequel 5](http://sequel.jeremyevans.net)
* [Erubi 1](https://github.com/jeremyevans/erubi)
* [MySQL 5.5](https://www.mysql.com)
* [Postgres 9.3](https://www.postgresql.org)

\* - Tests are developed but currently disabled due to compatibility issues.

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

* [Roda Google Group](http://groups.google.com/group/ruby-roda)
* `#roda` IRC Channel ([irc.freenode.net](irc://irc.freenode.net/roda))

### Resources

* [Roda Source Code](https://github.com/jeremyevans/roda)
