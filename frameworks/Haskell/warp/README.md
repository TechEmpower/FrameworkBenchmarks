# warp

This is the [`warp`](http://haskell-warp.github.io/) implementation of the [benchmarking test suite](https://www.techempower.com/benchmarks/) comparing a variety of web development platforms.

Since `warp` is strictly a request controller layer, it is upto the user to pick their persistance layer and data flow. Therefore we can have multiple distinct implementations using different database backends/libraries in each sub directory.
