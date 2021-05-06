# Express Benchmarking Test

This is the Express portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](app.js)

### Data-Store/Database Mapping Test

* [DB test controller/model](app.js)

## Infrastructure Software Versions
The tests were run with:
* [Node.js v14.15.0](http://nodejs.org/)
* [Express 4.17.1](http://expressjs.com/)

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
