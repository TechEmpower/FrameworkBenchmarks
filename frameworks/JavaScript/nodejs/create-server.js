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

  // Basic routes, no db required
  if (route === '/json') {
    return h.responses.jsonSerialization(req, res);
  } else if (route === '/plaintext') {
    return h.responses.plaintext(req, res);

  // No queries parameter required
  } else if (route === '/mongoose/db') {
    return MongooseHandler.SingleQuery(req, res);
  } else if (route === '/mongoose/fortunes') {
    return MongooseHandler.Fortunes(req, res);
  } else if (route === '/mongodb/db') {
    return MongodbRawHandler.SingleQuery(req, res);
  } else if (route === '/mongodb/fortunes') {
    return MongodbRawHandler.Fortunes(req, res);
  } else if (route === '/sequelize/db') {
    return SequelizeHandler.SingleQuery(req, res);
  } else if (route === '/sequelize/fortunes') {
    return SequelizeHandler.Fortunes(req, res);
  } else if (route === '/mysql/db') {
    return MySQLRawHandler.SingleQuery(req, res);
  } else if (route === '/mysql/fortunes') {
    return MySQLRawHandler.Fortunes(req, res);
  } else if (route === '/hiredis/db') {
    return HiredisHandler.SingleQuery(req, res);
  } else if (route === '/hiredis/fortunes') {
    return HiredisHandler.Fortunes(req, res);
  }

  else {
    var queries = isNaN(values.query.queries) ? 1 : parseInt(values.query.queries, 10);
    queries = Math.min(Math.max(queries, 1), 500);

    if (route === '/mongoose/queries') {
      return MongooseHandler.MultipleQueries(queries, req, res);
    } else if (route === '/mongoose/updates') {
      return MongooseHandler.Updates(queries, req, res);
    } else if (route === '/mongodb/queries') {
      return MongodbRawHandler.MultipleQueries(queries, req, res);
    } else if (route === '/mongodb/updates') {
      return MongodbRawHandler.Updates(queries, req, res);
    } else if (route === '/sequelize/queries') {
      return SequelizeHandler.MultipleQueries(queries, req, res);
    } else if (route === '/sequelize/updates') {
      return SequelizeHandler.Updates(queries, req, res);
    } else if (route === '/mysql/queries') {
      return MySQLRawHandler.MultipleQueries(queries, req, res);
    } else if (route === '/mysql/updates') {
      return MySQLRawHandler.Updates(queries, req, res);
    } else if (route === '/hiredis/queries') {
      return HiredisHandler.MultipleQueries(queries, req, res);
    } else if (route === '/hiredis/updates') {
      return HiredisHandler.Updates(queries, req, res);
    } else {
      return h.responses.routeNotImplemented(req, res);
    }
  }
}).listen(8080, function() {
  console.log("NodeJS worker listening on port 8080");
});
