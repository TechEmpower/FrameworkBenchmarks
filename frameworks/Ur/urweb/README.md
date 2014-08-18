TechEmpower Framework Benchmark: Ur/Web
=======================================

http://www.impredicative.com/ur/

To compile a standalone executable running on port 8080, run `urweb bench`.  See `setup.py` for fancier options.

`bench.ur` is the main source file.  `bench.urs` is the signature file describing the module's exported functions.  `bench.urp` is the project file giving compilation directives.

`benchmark_config` and `source_code` include metadata for the framework comparison.

`__init__.py` and `setup.py` are for starting and stopping the Ur/Web server.  `setup_mysql.py` is a variant using MySQL instead of PostgreSQL.
