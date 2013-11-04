TechEmpower Framework Benchmark: Ur/Web
=======================================

http://www.impredicative.com/ur/

To compile a standalone executable running on port 8080, run `urweb bench`.

`bench.ur` is the main source file. `bench.urs` is the signature file which describes the module's exported functions. `bench.urp` is the project file which gives compilation directives.

`fortune.sql` and `world.sql` are for the creation and population of the `uw_Bench_fortune` and `uw_Bench_world` tables. As part of its strong security guarantees, Ur/Web follows a certain naming convention for SQL tables which is incompatible with the default table names of `fortune` and `world`.

`benchmark_config` and `source_code` include metadata for the framework comparison.

`__init__.py` and `setup.py` are for starting and stopping the Ur/Web server.
