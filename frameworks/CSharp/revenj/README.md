# Revenj.NET on Mono and Windows

Revenj HTTP server + DB API + JSON API + PostgreSQL.
It uses precompiled DSL model for POCO classes.
It uses T4 Templates for HTML output

## DSL model
Data structures are defined in a DSL schema

 * [DSL source](Revenj.Bench/model.dsl)

## Test source

 * [C#](Revenj.Bench/RestService.cs)

## Test URLs

 * Plaintext - `http://localhost:8080/bench/plaintext`
 * JSON - `http://localhost:8080/bench/json`
 * DB - `http://localhost:8080/bench/db`
 * Queries - `http://localhost:8080/bench/queries/{count}`
 * Updates -  `http://localhost:8080/bench/updates/{count}`
 * Fortune -  `http://localhost:8080/bench/fortunes`

## Software Versions
The tests were run with:

 * [Mono 4.2](http://www.mono-project.com/)
 * [.NET 4.5](https://www.microsoft.com/net)
 * [Postgres 9.3](http://www.postgresql.org/)
 * [Revenj.NET 1.4.2](http://github.com/ngs-doo/revenj)
