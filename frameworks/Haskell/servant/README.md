# Servant

This is the [`servant`](http://haskell-servant.github.io/) implementation of a
[benchmarking test suite](https://www.techempower.com/benchmarks/) comparing a
variety of web development platforms.

Since `servant` is strictly a routing layer to typed function, it is upto the user to pick their persistance layer and data flow. Therefore we have multiple distinct implementations using different database backends/libraries.
