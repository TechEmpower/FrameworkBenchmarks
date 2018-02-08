# Grizzly Benchmarking Test

This is the Grizzly portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### Plain text serialization test
* [Plain text test resource](src/main/java/org/glassfish/grizzly/bm/PlainTextHttpHandler.java)

### JSON serialization test
* [JSON test resource](src/main/java/org/glassfish/grizzly/bm/JsonHttpHandler.java)

### Database tests
* N/A

### Fortunes test
* N/A

## Versions

* [Oracle Java 1.8](http://openjdk.java.net/)
* [Grizzly 2.3.34](http://grizzly.java.net/)
* [Jackson 2.9.3](http://wiki.fasterxml.com/JacksonHome)

## Test URLs

### Plain text test

    http://localhost:8080/plaintext

### JSON serialization test

    http://localhost:8080/json
