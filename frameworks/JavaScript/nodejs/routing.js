// Intialized database connections, one for each db config
// * Mongoose is a popular Node/MongoDB driver
// * Sequelize is a popular Node/SQL driver
const MongodbRawHandler = require('./handlers/mongodb-raw');
const MySQLRawHandler = require('./handlers/mysql-raw');
const MongooseHandler = require('./handlers/mongoose');
const SequelizeHandler = require('./handlers/sequelize');
const SequelizePgHandler = require('./handlers/sequelize-postgres');

const h = require('./helper');

module.exports.BasicHandler = ((() => {
  const self = {};

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

    '/sequelize-pg/db':       SequelizePgHandler.SingleQuery,
    '/sequelize-pg/fortunes': SequelizePgHandler.Fortunes
  };

  self.has = (path) => self.routes[path];

  self.handle = (path, req, res) => self.routes[path](req, res);

  return self;
})());

module.exports.QueryHandler = ((() => {
  const self = {};

  self.routes = {
    '/mongoose/queries':  MongooseHandler.MultipleQueries,
    '/mongoose/updates':  MongooseHandler.Updates,

    '/mongodb/queries':   MongodbRawHandler.MultipleQueries,
    '/mongodb/updates':   MongodbRawHandler.Updates,

    '/sequelize/queries': SequelizeHandler.MultipleQueries,
    '/sequelize/updates': SequelizeHandler.Updates,

    '/mysql/queries':     MySQLRawHandler.MultipleQueries,
    '/mysql/updates':     MySQLRawHandler.Updates,
    '/mysql/cached' :     MySQLRawHandler.CachedQueries,

    '/sequelize-pg/queries': SequelizePgHandler.MultipleQueries,
    '/sequelize-pg/updates': SequelizePgHandler.Updates
  };

  self.has = (path) => self.routes[path];

  self.handle = (path, queries, req, res) =>
    self.routes[path](queries, req, res);

  return self;
})());

