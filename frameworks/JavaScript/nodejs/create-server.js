// Forked workers will run this code when found to not be
// the master of the cluster.

var http = require('http');
var parseurl = require('parseurl'); // faster than native nodejs url package

// Initialize routes & their handlers (once)
var routing = require('./routing')
var basicHandler = routing.BasicHandler;
var queryHandler = routing.QueryHandler;
var routeNotImplemented = require('./helper').responses.routeNotImplemented;

module.exports = http.createServer(function (req, res) {
  var url = parseurl(req);
  var route = url.pathname;

  // Routes that do no require a `queries` parameter
  if (basicHandler.has(route)) {
    return basicHandler.handle(route, req, res);
  } else {
    // naive: only works if there is one query param, as is the case in TFB
    var queries = url.query && url.query.split('=')[1];
    queries = ~~(queries) || 1;
    queries = Math.min(Math.max(queries, 1), 500);

    if (queryHandler.has(route)) {
      return queryHandler.handle(route, queries, req, res);
    } else {
      return routeNotImplemented(req, res);
    }
  }

}).listen(8080, function() {
  console.log("NodeJS worker listening on port 8080");
});
