# Ktor with Vert.x async pgclient

This sets up testing using [Ktor](https://ktor.io/), with the async PostgreSQL client of the Eclipse Vert.x project.  
The client features batching, pipelining and supports coroutines.

## Test URLs

### Plain Text Test

    http://localhost:9090/plaintext

### JSON Encoding Test

    http://localhost:9090/json

### Single Query Test

    http://localhost:9090/db

### Multiple Queries Test

    http://localhost:9090/query?queries=

### Database updates Test

    http://localhost:9090/updates?queries=

### Fortunes Test

    http://localhost:9090/fortunes

## build

    ./gradlew build

## run

    java -jar build/libs/bench.jar
