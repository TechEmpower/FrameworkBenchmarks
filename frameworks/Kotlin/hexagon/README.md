
# Hexagon Benchmarking Test

This is the Hexagon portion of a [benchmarking test suite](../../../README.md) comparing a variety
of web development platforms. The test utilizes Hexagon routes and serialization.

## Tests

You can verify the benchmarks with the following command (from the project root):
`./tfb --mode verify --test hexagon hexagon-jetty hexagon-tomcat hexagon-netty hexagon-nettyepoll`

To run the full benchmarks locally, on the project root (not this directory) execute:
`./tfb --mode benchmark --test hexagon hexagon-jetty hexagon-tomcat hexagon-netty hexagon-nettyepoll`

## Infrastructure Software Versions

* [Hexagon stable version](http://hexagonkt.com)
