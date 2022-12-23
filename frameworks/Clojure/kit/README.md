# Kit Benchmarking Test

This is an implementation using the [Kit web framework](https://kit-clj.github.io/).

It uses PostgreSQL with [HikariCP](https://github.com/tomekw/hikari-cp)
and [next.jdbc](https://github.com/seancorfield/next-jdbc), [Selmer](https://github.com/yogthos/Selmer) for HTTP
templating, [Muuntaja](https://github.com/metosin/muuntaja) (with [jsonista](https://github.com/metosin/jsonista))
for content type coercion, [Undertow](https://github.com/luminus-framework/ring-undertow-adapter) for the web
server, [ring](https://github.com/ring-clojure/ring) and [reitit](https://github.com/metosin/reitit) for HTTP
abstraction and routing.

### Test Type Implementation Source Code

* [JSON](src/clj/io/github/kit_clj/te_bench/web/controllers/bench.clj#L85)
* [PLAINTEXT](src/clj/io/github/kit_clj/te_bench/web/controllers/bench.clj#L89)
* [DB](src/clj/io/github/kit_clj/te_bench/web/controllers/bench.clj#L94)
* [QUERY](src/clj/io/github/kit_clj/te_bench/web/controllers/bench.clj#L98)
* [CACHED QUERY](src/clj/io/github/kit_clj/te_bench/web/controllers/bench.clj#L102)
* [UPDATE](src/clj/io/github/kit_clj/te_bench/web/controllers/bench.clj#L111)
* [FORTUNES](src/clj/io/github/kit_clj/te_bench/web/controllers/bench.clj#L125)

Requirements
URL: https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#general-test-requirements

## Test URLs

### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=

### CACHED QUERY

http://localhost:8080/cached-queries?queries=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes
