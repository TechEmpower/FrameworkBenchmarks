# Ruby [Sinatra](http://www.sinatrarb.com/) Benchmarking Test

The information below contains information specific to Sinatra.
For further guidance, review the
[documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/).
Also note the additional information provided in the [Ruby README](../).

This is the Ruby Sinatra portion of a [benchmarking test suite](../../)
comparing a variety of web platforms.

## Infrastructure Software Versions
The tests were run with:
* [Ruby 2.2.3](http://www.ruby-lang.org/)
* [Sinatra 1.4.6](http://www.sinatrarb.com/)
* [Puma 2.15.3](http://puma.io/)
* [MySQL 5.5.29](https://dev.mysql.com/)
* [Sequel 4.28.0](http://sequel.jeremyevans.net/documentation.html)

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
