# Ruby [Sinatra](http://www.sinatrarb.com) Benchmarking Test

The information below contains information specific to the Sinatra benchmarking
test. For further guidance, review the
[documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/).  Also
note the additional information provided in the [Ruby README](../).

This is the Ruby Sinatra portion of a [benchmarking test suite](../../)
comparing a variety of web platforms.

## Infrastructure Software Versions

The tests will be run with:

* [Ruby 2.4](http://www.ruby-lang.org)
* [JRuby 9.1](http://jruby.org)\*
* [Rubinius 3](https://rubinius.com)\*
* [Puma 3.9](http://puma.io)
* [Passenger 5.1](https://www.phusionpassenger.com)
* [Unicorn 5.2](https://bogomips.org/unicorn/)
* [TorqueBox 4.0](http://torquebox.org)\*
* [Sinatra 2.0](http://www.sinatrarb.com)
* [ActiveRecord 5.0](https://github.com/rails/rails/tree/master/activerecord)
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

* [Sinatra Google Group](https://groups.google.com/forum/#!forum/sinatrarb)
* `#sinatra` IRC Channel ([irc.freenode.net](http://freenode.net/))
* `#sinatrarb` on the [Sinatra and Friends](http://sinatra-slack.herokuapp.com) Slack

### Resources

* [Sinatra Source Code](https://github.com/sinatra/sinatra)
