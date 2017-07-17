# Jooby Benchmarking Test

[Jooby](http://jooby.org) Scalable, fast and modular micro web framework for Java and Kotlin.

```java
public class App extends Jooby {

  {
    get("/", () -> "Hello, World!");
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

### Multiple Queries Test
* [Multiple queries test source](src/main/java/com/techempower/App.java)

### Database Update Test
* [Database update test source](src/main/java/com/techempower/App.java)

### Fortunes Test
* [Fortunes test source](src/main/java/com/techempower/App.java)

## Test URLs

### Plain Text Test

    http://localhost:8080/plaintext

### JSON Encoding Test

    http://localhost:8080/json

### Single Query Test

    http://localhost:8080/db

### Multiple Queries Test

    http://localhost:8080/queries

### Database updates Test

    http://localhost:8080/updates

### Fortunes Test

    http://localhost:8080/fortunes
