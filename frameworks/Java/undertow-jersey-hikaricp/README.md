# Undertow + Jersey + Hikaricp Benchmarking Test
### Heavily borrowed from the grizzly-jersey test

This is the Undertow+Jersey portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON serialization test
* [JSON test resource](src/main/java/hello/JsonResource.java)

### Database tests
* [Database tests resource](src/main/java/hello/DbResource.java)

### Fortunes test
* [Fortunes test resource](src/main/java/hello/FortunesResource.java)

## Test URLs

### JSON serialization test

    http://localhost:8080/json

### Database tests

    http://localhost:8080/db

    http://localhost:8080/db?queries=10

### Fortunes test

    http://localhost:8080/fortunes
