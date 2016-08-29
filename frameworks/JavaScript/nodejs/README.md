# NodeJS Benchmarking Test

This is the NodeJS portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### There are individual handlers for each DB approach
The logic for the test cases live in these.

* [MySQL raw](handlers/mysql-raw.js)
* [Sequelize (MySQL)](handlers/sequelize.js)
* [MongoDB raw](handlers/mongodb-raw.js)
* [Mongoose (MySQL)](handlers/mongoose.js)
* [Hiredis (Redis)](handlers/redis)

## Infrastructure Software Versions
The tests were run with:
* [Node.js v6.3.0](http://nodejs.org/)

* [Node MySQL 2.11.1](https://github.com/felixge/node-mysql/)
* [Sequelize 3.6.0](https://github.com/sequelize/sequelize)
* [Node MongoDB Driver 2.1.1](https://github.com/mongodb/node-mongodb-native)
* [Mongoose 4.5.3](http://mongoosejs.com/)
* [Node Redis 2.6.2](https://github.com/mranney/node_redis)
* [Hiredis 0.4.0 (C lib for Redis)](https://github.com/redis/hiredis)

## Test URLs

See the [Benchmark config](benchmark_config.json) file for a list of the tested routes.

`/json` and `/plaintext` are implemented

The four db-required tests, Single Query, Multiple Query, Fortunes, and Updates have been implemented for each of the 5 database approaches that this test covers
