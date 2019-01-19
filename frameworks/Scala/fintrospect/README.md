#fintrospect Benchmarking Test

## Infrastructure Software Versions
The tests were run with:

* [Java Oracle 1.8.0_25](http://www.oracle.com/technetwork/java/javase)
* [fintrospect 14.15.0](https://github.com/daviddenton/fintrospect)

## Test URLs

- JSON Encoding: http://localhost:9000/json
- Plaintext: http://localhost:9000/plaintext
- Fortunes: http://localhost:9000/fortunes
- Single Query: http://localhost:9000/db
- Multi Query: http://localhost:9000/queries?queries=1
- Update Query: http://localhost:9000/updates?queries=1

## How to run
sbt 'oneJar'

`java -jar target/scala-2.12/*fintrospect*one-jar.jar`

