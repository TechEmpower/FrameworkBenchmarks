# Jewelia Benchmark Test

[![Build Status](https://github.com/TechEmpower/FrameworkBenchmarks/workflows/build/badge.svg?branch=master&event=push)](https://github.com/TechEmpower/FrameworkBenchmarks/actions?query=workflow%3Abuild+branch%3Amaster)

This is a submission for [TechEmpower Framework Benchmarks (TFB)](http://www.techempower.com/benchmarks/) using the [Julia](https://julialang.org/) language.

All tests are located in [julia_server.jl](https://github.com/donavindebartolo/FrameworkBenchmarks/tree/master/frameworks/Julia/Jewelia).

### Implemented benchmarks
- [x] JSON serialization
- [x] Single query
- [x] Multiple queries
- [x] Plaintext
- [x] Fortunes
- [x] Updates

### 

This is an adaptation of the Jewelia benchmark, adapted for updated packages and Julia 1.9.

Introduces:
- Multi-Threading
- Connection pool the connection
- formatted with JuliaFormatter