# NodeJS Benchmarking Test

This is the NodeJS portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test controller/view](hello.js)

### Data-Store/Database Mapping Test

* [DB test controller/model](hello.js)

## Infrastructure Software Versions
The tests were run with:
* [Node.js v0.12.2](http://nodejs.org/)
* [Mongoose 4.0.1](http://mongoosejs.com/)
* [Sequelize 2.0.6](https://github.com/sequelize/sequelize)
* [Node MySQL 2.6.2](https://github.com/felixge/node-mysql/)
* [Node MongoDB Driver 2.0.27](https://github.com/mongodb/node-mongodb-native)

## Test URLs
### JSON Encoding Test

http://localhost:8080/json

### Plaintext Test

http://localhost:8080/plaintext

### Data-Store/Database Mapping Test

MongoDB:
http://localhost:8080/mongoose

MongoDB Raw:
http://localhost:8080/mongodb

MySQL:
http://localhost:8080/mysql-orm

MySQL Raw:
http://localhost:8080/mysql

### Variable Query Test

MongoDB:
http://localhost:8080/mongoose?queries=2

MongoDB Raw:
http://localhost:8080/mongodb?queries=2

MySQL:
http://localhost:8080/mysql-orm?queries=2

MySQL Raw:
http://localhost:8080/mysql?queries=2
