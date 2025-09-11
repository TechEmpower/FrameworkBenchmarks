# Avaje Jex Benchmarking Test

## Important Libraries
The tests were run with:
* [Java 24](https://openjdk.java.net)
* [Avaje Http 3.3](https://github.com/avaje/avaje-http)
* [Avaje Inject 11.5](https://github.com/avaje/avaje-inject)
* [Avaje Jex 3.2](https://github.com/avaje/avaje-jex)
* [HikariCP 6.3.0](https://github.com/brettwooldridge/HikariCP)
* [jstachio 1.3.7](https://github.com/jstachio/jstachio)

[Avaje Jex](https://avaje.io) is a micro web framework for Java's built-in `jdk.httpserver` API.

```java
    Jex.create()
        .get("/", ctx -> ctx.text("hello"))
        .start();
```

## Test Implementation Source Code

### Plain Text Test
* [Plain test source](src/main/java/benchmark/Main.java)

### JSON Encoding Test
* [JSON test source](src/main/java/benchmark/Main.java)

### Single Query Test
* [Single query test source](src/main/java/benchmark/DatabaseController.java)

### Multiple Queries Test
* [Multiple queries test source](src/main/java/benchmark/DatabaseController.java)

### Database Update Test
* [Database update test source](src/main/java/benchmark/DatabaseController.java)

### Fortunes Test
* [Fortunes test source](src/main/java/benchmark/DatabaseController.java)

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/queries?queries=

### UPDATE

http://localhost:8080/updates?queries=

### FORTUNES

http://localhost:8080/fortunes

## build

### java's httpserver impl

 `mvn clean package`

### robaho's httpserver impl

 `mvn clean package -P robaho`

### jetty's httpserver impl

 `mvn clean package -P jetty`
    
## run

 `java -p ./target/modules/ -m avaje.techempower`