# Jooby Benchmarking Test

[Jooby](http://jooby.org) is a micro-web framework for Java.

```java
public class App extends Jooby {

  {
    get("/plaintext", () -> "Hello, World!");
  }

}
```

This is the [Jooby](http://jooby.org) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.


### Plain Text Test
* [Plain test source](src/main/java/com/techempower/App.java)

### JSON Encoding Test
* [JSON test source](src/main/java/com/techempower/App.java)

### Single Query Test
* [Single query test source](src/main/java/com/techempower/App.java)

## Versions

* [Jooby 0.11.x](http://jooby.org)
* [Undertow 1.2.x](http://undertow.io)
* [Jackson 2.6.x](http://wiki.fasterxml.com/JacksonHome)

## Test URLs

### Plain Text Test

    http://localhost:8080/plaintext

### JSON Encoding Test

    http://localhost:8080/json

### Single Query Test

    http://localhost:8080/db

