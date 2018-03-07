# Minijax Benchmarking Test

[Minijax](https://minijax.org) - Lightweight subset of JAX-RS and JSR 330.

* Undertow for HTTP server
* Mustache for HTML templates
* MySQL for database
* Eclipselink for ORM

All REST endpoints are in [MinijaxBenchmark.java](src/main/java/com/techempower/minijax/MinijaxBenchmark.java)

## Test URLs

All implementations use the same URLs.

 * Plaintext - `http://localhost:8080/plaintext`
 * JSON - `http://localhost:8080/json`
 * DB - `http://localhost:8080/db`
 * Queries - `http://localhost:8080/db?queries=`
 * Updates - `http://localhost:8080/updates?queries=`
 * Fortune - `http://localhost:8080/fortunes`
