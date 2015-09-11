# BeyondJ Benchmarking Test

This is the BeyondJ portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

This is using several [BeyondJ projects](http://beyondj.com/dist/docs/README.html) to build an application.

## How to run this app locally

You can easily run this sample application 
For that, simply run:

    java -jar beyondj.jar

## Guides
* [Building a service](http://beyondj.com/dist/docs/README.html)

## Documentation
See [BeyondJ projects documentation](http://beyondj.com/dist/docs/README.html).

## Application Endpoints

When deployed locally, the application uses the 'ROOT' context; for the benchmark, all
URLs are prefixed with "/beyondj".
Check out [SampleApplication, the main Application file](src/main/java/com/techempower/beyondj/SampleApplication.java)

### JSON serialization

* http://localhost:8080/servlets/performance?op=json
* [JSON Controller](src/main/java/com/techempower/beyondj/web/HelloController.java)

### Single database query

* http://localhost:8080/db
* [Database Controller](src/main/java/com/techempower/beyondj/web/WorldDatabaseController.java)
* [Database Entity](src/main/java/com/techempower/beyondj/domain/World.java)
* [Database Repository](src/main/java/com/techempower/beyondj/service/WorldRepository.java)

### Multiple database queries

* http://localhost:8080/servlets/performance?queries=5
* [Database Controller](src/main/java/com/techempower/beyondj/web/WorldDatabaseController.java)

### Fortunes

* http://localhost:8080/servlets/performance?op=fortunes
* [Fortune Controller](src/main/java/com/techempower/beyondj/web/FortuneController.java)

### Database updates

* http://localhost:8080//servlets/performance?op=updates&queries=5
* [Database Controller](src/main/java/com/techempower/beyondj/web/WorldDatabaseController.java)

### Plaintext

* http://localhost:8080/servlets/performance?op=plaintext
* [Controller](src/main/java/com/techempower/beyondj/web/HelloController.java)


## Infrastructure Software Versions
The tests were run with:

* [BeyondJ 1.0-SNAPSHOT](http://beyondj.com/dist/docs/README.html)
* [Java HotSpot 1.8.0_20]
