# NodeJS Benchmarking Test

This is the NodeJS portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

### There are individual handlers for each DB approach
The logic for the test cases live in these.

* [MySQL raw](handlers/mysql-raw.js)
* [Sequelize (MySQL)](handlers/sequelize.js)
* [MongoDB raw](handlers/mongodb-raw.js)
* [Mongoose (MySQL)](handlers/mongoose.js)

### Alternative server Test
This test suite also contains tests using an alternative http server. [Mitol](https://github.com/Helidium/Mitol) is a NodeJS addon written in C++.
The server is currently in Alpha state, but aims to be a drop-in replacement for high performance demands.

## Infrastructure Software Versions
The tests were run with:
* [Node.js v14.17.1](http://nodejs.org/)

* [Node MySQL 2.16.0](https://github.com/felixge/node-mysql/)
* [Sequelize 5.15.1](https://github.com/sequelize/sequelize)
* [Node MongoDB Driver 2.2.33](https://github.com/mongodb/node-mongodb-native)
* [Mongoose 5.7.5](http://mongoosejs.com/)

## Test URLs

See the [Benchmark config](benchmark_config.json) file for a list of the tested routes.

`/json` and `/plaintext` are implemented

The four db-required tests, Single Query, Multiple Query, Fortunes, and Updates have been implemented for each of the 5 database approaches that this test covers.
