// Intialized database connections, one for each db config
// * Mongoose is a popular Node/MongoDB driver
// * Sequelize is a popular Node/SQL driver
//const MongodbRawHandler = require('./handlers/mongodb-raw');
//const MySQLRawHandler = require('./handlers/mysql-raw');
//const MongooseHandler = require('./handlers/mongoose');
//const SequelizeHandler = require('./handlers/sequelize');
//const SequelizePgHandler = require('./handlers/sequelize-postgres');

const h = require('./helper');

module.exports.BasicHandler = ((() => {
  const self = {};

  self.routes = {
    '/json':               h.responses.jsonSerialization,
    '/plaintext':          h.responses.plaintext,

    
  };

  self.has = (path) => self.routes[path];

  self.handle = (path, req, res) => self.routes[path](req, res);

  return self;
})());

module.exports.QueryHandler = ((() => {
  const self = {};

  self.routes = {
    
  };

  self.has = (path) => self.routes[path];

  self.handle = (path, queries, req, res) =>
    self.routes[path](queries, req, res);

  return self;
})());

