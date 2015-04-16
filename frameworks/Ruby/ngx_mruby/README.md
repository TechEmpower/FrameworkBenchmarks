# [nginx + mruby](https://github.com/matsumoto-r/ngx_mruby) Benchmark Test

The information below contains information specific to nginx + mruby. 
For further guidance, review the 
[documentation](http://frameworkbenchmarks.readthedocs.org/en/latest/). 
Also note the additional information provided in the [Ruby README](../).

This is the nginx + mruby portion of a [benchmarking test suite](../../) 
comparing a variety of web platforms.

---

The nginx app is distributes across a few files. most of it is in [nginx.conf](nginx.conf)
The nginx conf is inside [nginx.conf](nginx.conf)
Requires a nginx compiled with ngx_mruby module. It has been called nginx_mruby for lack of a better name

## Infrastructure Software Versions

The tests were run with:

* [Ruby 2.0.0-p0](http://www.ruby-lang.org/)

## Paths & Source for Tests

* [JSON Serialization](nginx.conf): "/json"
* [Single Database Query](db.rb): "/db"
* [Multiple Database Query](queries.rb): "/queries?queries={#}"
* _Fortunes: N/A_
* _Database Updates: N/A_
* [Plaintext](nginx.conf): "/plaintext"

## Get Help

### Experts

_No experts listed, yet. If you're an expert, add yourself!_
