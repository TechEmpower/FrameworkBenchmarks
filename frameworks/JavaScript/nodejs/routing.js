// Intialized database connections, one for each db config
// * Mongoose is a popular Node/MongoDB driver
// * Sequelize is a popular Node/SQL driver
// * Node's redis package uses the C bindings of the hiredis library
var MongodbRawHandler = require('./handlers/mongodb-raw');
var MySQLRawHandler = require('./handlers/mysql-raw');
var MongooseHandler = require('./handlers/mongoose');
var SequelizeHandler = require('./handlers/sequelize');
var HiredisHandler = require('./handlers/redis');
var SequelizePgHandler = require('./handlers/sequelize-postgres');

var h = require('./helper');

module.exports.BasicHandler = (function() {
  var self = {}

  self.routes = {
    '/json':               h.responses.jsonSerialization,
    '/plaintext':          h.responses.plaintext,

    '/mongoose/db':        MongooseHandler.SingleQuery,
    '/mongoose/fortunes':  MongooseHandler.Fortunes,

    '/mongodb/db':         MongodbRawHandler.SingleQuery,
    '/mongodb/fortunes':   MongodbRawHandler.Fortunes,

    '/sequelize/db':       SequelizeHandler.SingleQuery,
    '/sequelize/fortunes': SequelizeHandler.Fortunes,

    '/mysql/db':           MySQLRawHandler.SingleQuery,
    '/mysql/fortunes':     MySQLRawHandler.Fortunes,

    '/hiredis/db':         HiredisHandler.SingleQuery,
    '/hiredis/fortunes':   HiredisHandler.Fortunes,

    '/sequelize-pg/db':       SequelizePgHandler.SingleQuery,
    '/sequelize-pg/fortunes': SequelizePgHandler.Fortunes
  }

  self.has = function(path) {
    return self.routes[path];
  }

  self.handle = function(path, req, res) {
    return self.routes[path](req, res);
  }

  return self;
}());

module.exports.QueryHandler = (function () {
  var self = {}

  self.routes = {
    '/mongoose/queries':  MongooseHandler.MultipleQueries,
    '/mongoose/updates':  MongooseHandler.Updates,

    '/mongodb/queries':   MongodbRawHandler.MultipleQueries,
    '/mongodb/updates':   MongodbRawHandler.Updates,

    '/sequelize/queries': SequelizeHandler.MultipleQueries,
    '/sequelize/updates': SequelizeHandler.Updates,

    '/mysql/queries':     MySQLRawHandler.MultipleQueries,
    '/mysql/updates':     MySQLRawHandler.Updates,

    '/hiredis/queries':   HiredisHandler.MultipleQueries,
    '/hiredis/updates':   HiredisHandler.Updates,

    '/sequelize-pg/queries': SequelizePgHandler.MultipleQueries,
    '/sequelize-pg/updates': SequelizePgHandler.Updates
  }

  self.has = function(path) {
    return self.routes[path];
  }

  self.handle = function(path, queries, req, res) {
    return self.routes[path](queries, req, res);
  }

  return self;
}());

