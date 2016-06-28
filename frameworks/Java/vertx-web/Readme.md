# Vert.x Web Benchmarking Test

This is the Vert.x Web portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Test URLs

### Plain Text Test

http://localhost:8080/plaintext

### JSON Encoding Test

http://localhost:8080/json

### Data-Store/Database Mapping Test

http://localhost:8080/mongo/db
http://localhost:8080/mongo/queries?queries=5

http://localhost:8080/jdbc/db
http://localhost:8080/jdbc/queries?queries=5

### Data-Store/Database Update Test

http://localhost:8080/mongo/update?queries=5

http://localhost:8080/jdbc/update?queries=5

### Template rendering Test

http://localhost:8080/mongo/fortunes

http://localhost:8080/jdbc/fortunes

## Generating Load
It's best to generate load from a completely separate machine from the server if you can, to avoid resource contention
during the test.

We use the [wrk](https://github.com/wg/wrk) load generation tool to generate the load for our benchmark runs. It's the
best tool we've found for the job and supports HTTP pipelining (used by the plaintext scenario) via its scripting
interface. Wrk will only run from a Linux machine however, so if you must use Windows, try using
[ab](https://httpd.apache.org/docs/2.2/programs/ab.html) (Apache Bench).

You'll need to clone the [wrk repo](https://github.com/wg/wrk) on your load generation machine and follow
[their instructions to build it](https://github.com/wg/wrk/wiki/Installing-Wrk-on-Linux).

Here's a sample wrk command to generate load for the JSON scenario. This run is using 256 connections across 32 client
threads for a duration of 10 seconds.

```
wrk -c 256 -t 32 -d 10 http://127.0.0.1:8080/json
```

To generate pipelined load for the plaintext scenario, use the following command, assuming your CWD is the root of this
repo and wrk is on your path. The final argument after the `--` is the desired pipeline depth. We always run the
plaintext scenario at a pipeline depth of 16, [just like the Techempower Benchmarks](https://github.com/TechEmpower/FrameworkBenchmarks/blob/6594d32db618c6ca65e0106c5adf2671f7b63654/toolset/benchmark/framework_test.py#L640).

```
wrk -c 256 -t 32 -d 15 -s ./scripts/pipeline.lua http://localhost:8080/psql/update?queries=20 -- 16
```

*Note you may want to tweak the number of client threads (the `-t` arg) being used based on the specs of your load
generation machine.*
