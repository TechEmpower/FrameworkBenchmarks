# Ruby [Rack](http://rack.github.io/) Benchmarking Test

The information below contains information specific to Rack.
For further guidance, review the
[documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki).
Also note the additional information provided in the [Ruby README](../).

This is the Ruby Rack portion of a [benchmarking test suite](../../)
comparing a variety of web servers.

## Infrastructure Software Versions
The tests were run with:

* [Ruby 3.2](http://www.ruby-lang.org/)
* [JRuby 9.4](http://jruby.org/)
* [Rack 3.0.7](http://rack.github.com/)
* [Unicorn 6.1.0](http://unicorn.bogomips.org/)
* [Puma 6.2.1](http://puma.io/)
* [Falcon 0.42.3](https://github.com/socketry/falcon)
* [Sequel 5.68.0](https://sequel.jeremyevans.net/)



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

