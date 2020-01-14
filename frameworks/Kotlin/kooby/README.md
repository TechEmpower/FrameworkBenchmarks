# Kooby (Jooby + Kotlin)

[Jooby](https://jooby.io) the modular micro web framework for Java and Kotlin.

```kt
import io.jooby.runApp

fun main(args: Array<String>) {
  runApp(args) {

    get("/") {
      "Hello Kooby!"
    }

  }
}

```

This is the [Jooby](https://jooby.io) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

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

## build

    mvn clean package
    
## run

    java -jar target/kooby.jar
