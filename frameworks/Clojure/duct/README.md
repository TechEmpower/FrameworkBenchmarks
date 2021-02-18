# Duct Benchmarking Test

Implementation of the benchmark using the Clojure [Duct framework](https://github.com/duct-framework).

## Test Type Implementation Source Code

* [JSON](resources/hello/config.edn)
* [PLAINTEXT](resources/hello/config.edn)
* [DB](src/hello/handler/single_query.clj)
* [QUERY](src/hello/handler/queries.clj)
* [FORTUNES](src/hello/handler/fortunes.clj)

## Important Libraries
The tests were run with these versions:

```edn
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [duct/core "0.6.2"]
                 [duct/module.logging "0.3.1"]
                 [duct/module.web "0.6.4"]
                 [duct/module.ataraxy "0.2.0"]
                 [duct/module.sql "0.4.2"]
                 [duct/handler.sql "0.3.1"]
                 [duct/database.sql.hikaricp "0.3.3"]
                 [hiccup "1.0.5"]
                 ; alternate servers
                 [duct/server.http.http-kit "0.1.2"]
                 [duct/server.http.aleph "0.1.2"]
                 [me.grison/duct-immutant "0.1.0"]
                 ; tested databases
                 [org.postgresql/postgresql "42.1.4"]
                 [me.grison/duct-mongodb "0.1.1"]]
```

See **project.clj** for more information.

## Database

For the moment it test both PostgreSQL and MongoDB.

Implementation for *Jetty*, *Aleph*, *HTTP-Kit* and *Immutant* are part of this project.

## Test URLs
### JSON

[http://localhost:3000/json](http://localhost:3000/json)

### PLAINTEXT

[http://localhost:3000/plaintext](http://localhost:3000/plaintext)

### DB

[http://localhost:3000/db](http://localhost:3000/db)
[http://localhost:3000/db-mongo](http://localhost:3000/db-mongo)

### QUERY

[http://localhost:3000/queries?queries=](http://localhost:3000/queries?queries=)
[http://localhost:3000/queries-mongo?queries=](http://localhost:3000/queries-mongo?queries=)

### FORTUNES

[http://localhost:3000/fortunes](http://localhost:3000/fortunes)
[http://localhost:3000/fortunes-mongo](http://localhost:3000/fortunes-mongo)

## Contributors

- Alexandre Grison - [agrison](https://github.com/agrison) - [@algrison](https://twitter.com/algrison)

