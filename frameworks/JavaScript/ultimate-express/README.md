# Ultimate-Express Benchmarking Test

This is the Ultimate Express portion of a [benchmarking test suite](../) comparing a variety of web development platforms.
The Ultimate Express. Fastest http server with full Express compatibility, based on µWebSockets.

### JSON Encoding Test

* [JSON test source](app.js)

### Data-Store/Database Mapping Test

* [DB test controller/model](app.js)

## Infrastructure Software Versions
The tests were run with:
* [Node.js v16.13.0](http://nodejs.org/)
* [µExpress / Ultimate Express 1.3.7](https://github.com/dimdenGD/ultimate-express)

## Resources
* http://nodejs.org/api/cluster.html

## Test URLs
### JSON Encoding Test

http://localhost:8080/json

### Data-Store/Database Mapping Test

MongoDB:
http://localhost:8080/mongoose

MySQL:
http://localhost:8080/sequelize

PostgreSQL:
http://localhost:8080/db

### Variable Query Test

MongoDB:
http://localhost:8080/mongoose?queries=2

MySQL:
http://localhost:8080/sequelize?queries=2

PostgreSQL:
http://localhost:8080/queries?queries=2
