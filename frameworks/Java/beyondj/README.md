# BeyondJ Benchmarking Test

This is the BeyondJ portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

This is using several [BeyondJ projects](http://beyondj.com/dist/docs/README.html) to build an application.

## How to run this app

You can easily run this sample application 
For that, simply run:

    java -jar beyondj-launcher/target/beyondj.jar &

## Guides
* [Building a service](http://beyondj.com/dist/docs/README.html)

## Documentation
See [BeyondJ projects documentation](http://beyondj.com/dist/docs/README.html).

## Application Endpoints

The application uses the 'beyondj-service' context;
For the benchmark, all URLs are prefixed with "/perf"
Check out [SampleApplication, the main Application file]
(beyondj-launcher/src/main/java/com/techempower/beyondj/BeyondJApplication.java)

### JSON serialization

* http://localhost:8080/beyondj-service/perf/hello
* [JSON ActionBean](beyondj-service/src/main/java/com/techempower/beyondj/action/HelloActionBean.java)

### Single database query

* http://localhost:8080/beyondj-service/perf/database/db
* [Database ActionBean](beyondj-service/src/main/java/com/techempower/beyondj/action/WorldDatabaseActionBean.java)
* [Database Entity](beyondj-data/src/main/java/com/techempower/beyondj/domain/World.java)
* [Database Repository](beyondj-data/src/main/java/com/techempower/beyondj/repository/WorldRepositoryImpl.java)

### Multiple database queries

* http://localhost:8080/beyondj-service/perf/database/queries?&queries=5
* [Database ActionBean](beyondj-service/src/main/java/com/techempower/beyondj/action/WorldDatabaseActionBean.java)

### Fortunes

* http://localhost:8080/beyondj-service/perf/fortunes
* [Fortune ActionBean](beyondj-service/src/main/java/com/techempower/beyondj/action/FortuneActionBean.java)

### Database updates

* http://localhost:8080/beyondj-service/perf/database/updates?&queries=5
* [Database ActionBean](beyondj-service/src/main/java/com/techempower/beyondj/action/WorldDatabaseActionBean.java)

### Plaintext

* http://localhost:8080/beyondj-service/perf/hello/plaintext
* [ActionBean](beyondj-service/src/main/java/com/techempower/beyondj/action/HelloActionBean.java)


## Infrastructure Software Versions
The tests were run with:

* [BeyondJ 1.0](http://beyondj.com/dist/docs/README.html)
* [Java HotSpot 1.8.0_20]


Note: the code was adopted from the Spring version of the benchmarks
