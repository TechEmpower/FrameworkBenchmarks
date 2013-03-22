# NodeJS Benchmarking Test

This is the NodeJS portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test controller/view](hello.js)

### Data-Store/Database Mapping Test

* [DB test controller/model](hello.js)

## Infrastructure Software Versions
The tests were run with:
* [Node.js v0.10.0](http://nodejs.org/)
* [Mongoose 3.5.5](http://mongoosejs.com/)
* [Sequelize 1.6.0-beta4](http://www.sequelizejs.com/)
* [MySQL 5.5.29](https://dev.mysql.com/)

## Test URLs
### JSON Encoding Test

http://localhost:8080/json

### Data-Store/Database Mapping Test

MongoDB:
http://localhost:8080/mongoose

MySQL:
http://localhost:8080/sequelize

MySQL Raw:
http://localhost:8080/mysql

### Variable Query Test

MongoDB:
http://localhost:8080/mongoose?queries=2

MySQL:
http://localhost:8080/sequelize?queries=2

MySQL Raw:
http://localhost:8080/mysql?queries=2
