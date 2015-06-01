// Forked workers will run this code when found to not be
// the master of the cluster.

var http = require('http');
var url = require('url');
var h = require('./helper');

// Handlers, one for each db config
var MongodbRawHandler = require('./handlers/mongodb-raw');
var MySQLRawHandler = require('./handlers/mysql-raw');

// Mongoose is a popular Node/MongoDB driver
var MongooseHandler = require('./handlers/mongoose');

// Sequelize is a popular Node/SQL driver
var SequelizeHandler = require('./handlers/sequelize');

// Node's redis package uses the C bindings of the hiredis library
var HiredisHandler = require('./handlers/redis');

module.exports = http.createServer(function (req, res) {
  var values = url.parse(req.url, true);
  var route = values.pathname;

  var basicHandlers = {
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
    '/hiredis/fortunes':   HiredisHandler.Fortunes
  }

  if (basicHandlers[route]) {
    return basicHandlers[route](req, res);
  } else {
    var queries = ~~(values.query.queries) || 1;
    queries = Math.min(Math.max(queries, 1), 500);

    var queriesHandlers = {
      '/mongoose/queries':  MongooseHandler.MultipleQueries,
      '/mongoose/updates':  MongooseHandler.Updates,

      '/mongodb/queries':   MongodbRawHandler.MultipleQueries,
      '/mongodb/updates':   MongodbRawHandler.Updates,

      '/sequelize/queries': SequelizeHandler.MultipleQueries,
      '/sequelize/updates': SequelizeHandler.Updates,

      '/mysql/queries':     MySQLRawHandler.MultipleQueries,
      '/mysql/updates':     MySQLRawHandler.Updates,

      '/hiredis/queries':   HiredisHandler.MultipleQueries,
      '/hiredis/updates':   HiredisHandler.Updates
    }

    if (queriesHandlers[route]) {
      return queriesHandlers[route](queries, req, res);
    } else {
      return h.responses.routeNotImplemented(req, res);
    }
  }

}).listen(8080, function() {
  console.log("NodeJS worker listening on port 8080");
});
