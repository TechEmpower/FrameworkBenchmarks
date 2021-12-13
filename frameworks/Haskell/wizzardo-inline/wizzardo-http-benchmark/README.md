This is the implementation of an Http server using
[wizzardo-http][wizzardo-http] with Haskell callbacks using
[inline-java][inline-java].

Furtheremore, an experimental interface of inline-java is used, where
[-XLinearTypes][linear-types] ensures references to Java objects are
handled correctly on the Haskell side.

Running the benchmark requires the framework from
https://github.com/TechEmpower/FrameworkBenchmarks
