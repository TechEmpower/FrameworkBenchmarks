// Intialized database connections, one for each db config
// * Mongoose is a popular Node/MongoDB driver
// * Sequelize is a popular Node/SQL driver

const Handler = require(`./handlers/${process.env.NODE_HANDLER}`);
const h = require('./helper');

module.exports.BasicHandler = ((() => {
  const self = {};

  self.routes = {
    '/json':               h.responses.jsonSerialization,
    '/plaintext':          h.responses.plaintext,
    '/db':        Handler.SingleQuery,
    '/fortunes':  Handler.Fortunes,
  };

  self.has = (path) => self.routes[path];

  self.handle = (path, req, res) => self.routes[path](req, res);

  return self;
})());

module.exports.QueryHandler = ((() => {
  const self = {};

  self.routes = {
    '/queries':     Handler.MultipleQueries,
    '/updates':     Handler.Updates,
    '/cached' :     Handler.CachedQueries,
  };

  self.has = (path) => self.routes[path];

  self.handle = (path, queries, req, res) =>
    self.routes[path](queries, req, res);

  return self;
})());

