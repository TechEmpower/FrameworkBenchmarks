# Ruby [Rack](http://rack.github.io/) Benchmarking Test

The information below contains information specific to Rack.
For further guidance, review the
[documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note the additional information provided in the [Ruby README](../).

This is the Ruby Rack portion of a [benchmarking test suite](../../)
comparing a variety of web servers.

## Infrastructure Software Versions
The tests were run with:

* [Ruby 3.4](http://www.ruby-lang.org/)
* [JRuby 9.4](http://jruby.org/)
* [Rack 3](http://rack.github.com/)
* [Falcon](https://github.com/socketry/falcon)
* [Iodine](https://github.com/boazsegev/iodine)
* [Itsi](https://github.com/wouterken/itsi)
* [Passenger](https://github.com/phusion/passenger)
* [Pitchfork](https://github.com/Shopify/pitchfork)
* [Puma](http://puma.io/)
* [Sequel 5](https://sequel.jeremyevans.net/)


## Paths & Source for Tests

* Routing and controller logic is in hello_world.rb
* Database access is done with pg_db.rb (only postgres is supported and we are using sequel to connect and run queries)
* No ORM is used.

## Get Help

### Experts

* Samuel Williams (@ioquatix) -- Async & Falcon developer.
* Tim Uckun (@timuckun)


### Resources

* [Rack Source Code](https://github.com/rack/rack)

