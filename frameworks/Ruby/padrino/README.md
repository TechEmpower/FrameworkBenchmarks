# Padrino Framework

The information below contains information specific to Padrino.
For further guidance, review the
[documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note the additional information provided in the [Ruby README](../).

This is the Ruby Padrino portion of a [benchmarking test suite](../../)
comparing a variety of web servers.

## Infrastructure Software Versions
The tests were run with:

* [Ruby 3.5](http://www.ruby-lang.org/)
* [Padrino 0.12.3](http://www.padrinorb.com/)
* [Iodine](https://github.com/boazsegev/iodine)

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
