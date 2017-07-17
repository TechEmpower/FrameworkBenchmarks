
/**
 * Module dependencies.
 */

const cluster = require('cluster'),
  numCPUs = require('os').cpus().length,
  express = require('express'),
  async = require('async');

const bodyParser = require('body-parser'),
  methodOverride = require('method-override'),
  errorHandler = require('errorhandler');

if (cluster.isMaster) {
  // Fork workers.
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) =>
    console.log('worker ' + worker.pid + ' died'));
} else {
  const app = module.exports = express();

  // Configuration
  // https://github.com/expressjs/method-override#custom-logic
  app.use(bodyParser.urlencoded({ extended: true }));
  app.use(methodOverride((req, res) => {
    if (req.body && typeof req.body === 'object' && '_method' in req.body) {
      // look in urlencoded POST bodies and delete it
      const method = req.body._method;
      delete req.body._method;
      return method;
    }
  }));

  // Set headers for all routes
  app.use((req, res, next) => {
    res.setHeader("Server", "Express");
    return next();
  });

  app.set('view engine', 'jade');
  app.set('views', __dirname + '/views');

  // Check Node env.
  const env = process.env.NODE_ENV || 'development';
  if ('development' == env) {
    app.use(errorHandler({ dumpExceptions: true, showStack: true }));
  }
  if ('production' == env) {
    app.use(errorHandler());
  }

  // Routes
  app.get('/json', (req, res) => res.send({ message: 'Hello, World!' }));

  app.get('/plaintext', (req, res) =>
    res.header('Content-Type', 'text/plain').send('Hello, World!'));

  app.listen(8080);
}
