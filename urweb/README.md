TechEmpower Framework Benchmark: Ur/Web
=======================================

This is a benchmark solution in the Ur/Web language:
  http://www.impredicative.com/ur/

Caveats, in Comparing with Other Frameworks
===========================================

Ur/Web is unusual in a few ways of favoring simplicity over performance, which has consequences for the results in this benchmark comparison.  For instance:
* *Security*: The design of Ur/Web favors making common security mistakes impossible.  Most such mechanisms have no run-time costs, but some do.  For instance, the standard-library random number generator is cryptographically secure, which adds overhead to the benchmarks that generate many random numbers.
* *Concurrency*: The concurrency model of Ur/Web is transactions.  An Ur/Web programmer never needs to think about an interleaving semantics for threads.  The implementation relies on the transaction features of SQL engines, which add an overhead (e.g., extra database round-trips to begin/end transactions) to those tests that most other solutions avoid by accepting more complex concurrency semantics.  (We hope there will be an explicit transactions test soon!)

Code Structure & Build Instructions
===================================

To compile a standalone executable running on port 8080, run `urweb bench`.  See `setup.py` for fancier options.

`bench.ur` is the main source file.  `bench.urs` is the signature file describing the module's exported functions.  `bench.urp` is the project file giving compilation directives.

`benchmark_config` and `source_code` include metadata for the framework comparison.

`__init__.py` and `setup.py` are for starting and stopping the Ur/Web server.  `setup_mysql.py` is a variant using MySQL instead of PostgreSQL.
