# Ruby [Rack](http://rack.rubyforge.org)-[Sequel](http://sequel.jeremyevans.net) Benchmarking Test

The information below contains information specific to the Sequel rewrite of
the Rack benchmarking test. For further guidance, review the
[documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).  Also
note the additional information provided in the [Ruby README](../).

This is the Ruby Rack portion of a [benchmarking test suite](../../) comparing
a variety of web platforms.

## Infrastructure Software Versions

The tests will be run with:

* [Ruby 3.4](http://www.ruby-lang.org)
* [Puma 7](http://puma.io)
* [Rack 3](http://rack.rubyforge.org)
* [Sequel 5](http://sequel.jeremyevans.net)
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

* [Rack Google Group](https://groups.google.com/forum/#!forum/rack-devel)
* `#rack` IRC Channel ([irc.freenode.net](http://freenode.net/))

### Resources

* [Rack Source Code](https://github.com/rack/rack)
