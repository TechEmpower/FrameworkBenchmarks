
/**
 * Module dependencies.
 */

const cluster = require('cluster'),
  numCPUs = require('os').cpus().length,
  express = require('ultimate-express');

if (cluster.isPrimary) {
  console.log(`Primary ${process.pid} is running`);

  // Fork workers.
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) => {
    console.log(`worker ${worker.process.pid} died`);
  });
} else {
  const app = module.exports = express();
  app.set("etag", false);
  app.set('view engine', 'jade');
  app.set('views', __dirname + '/views');

  // Routes
  app.get('/json', (req, res) => {
    res.setHeader("Server", "UltimateExpress");
    res.send({ message: 'Hello, World!' });
  });

  app.get('/plaintext', (req, res) => {
    res.setHeader('Content-Type', 'text/plain');
    res.setHeader("Server", "UltimateExpress");
    res.send('Hello, World!');
  });

  // Configuration
  app.use(express.urlencoded({ extended: true }));

  // Set headers for all routes
  app.use((req, res, next) => {
      res.setHeader("Server", "UltimateExpress");
      return next();
  });

  app.listen(8080);
}
