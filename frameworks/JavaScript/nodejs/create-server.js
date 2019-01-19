// Forked workers will run this code when found to not be
// the master of the cluster.

const http = require('http');
const parseurl = require('parseurl'); // faster than native nodejs url package

// Initialize routes & their handlers (once)
const routing = require('./routing');
const basicHandler = routing.BasicHandler;
const queryHandler = routing.QueryHandler;
const routeNotImplemented = require('./helper').responses.routeNotImplemented;

module.exports = http.createServer(function (req, res) {
  const url = parseurl(req);
  const route = url.pathname;

  // Routes that do no require a `queries` parameter
  if (basicHandler.has(route)) {
    return basicHandler.handle(route, req, res);
  } else {
    // naive: only works if there is one query param, as is the case in TFB
    let queries = url.query && url.query.split('=')[1];
    queries = ~~(queries) || 1;
    queries = Math.min(Math.max(queries, 1), 500);

    if (queryHandler.has(route)) {
      return queryHandler.handle(route, queries, req, res);
    } else {
      return routeNotImplemented(req, res);
    }
  }

}).listen(8080, () => console.log("NodeJS worker listening on port 8080"));
