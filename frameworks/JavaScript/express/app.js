
/**
 * Module dependencies.
 */

const cluster = require('cluster'),
  numCPUs = require('os').cpus().length,
  express = require('express');

const bodyParser = require('body-parser');

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

  // Configuration
  app.use(bodyParser.urlencoded({ extended: true }));

  // Set headers for all routes
  app.use((req, res, next) => {
    res.setHeader("Server", "Express");
    return next();
  });

  app.set('view engine', 'jade');
  app.set('views', __dirname + '/views');

  // Routes
  app.get('/json', (req, res) => res.send({ message: 'Hello, World!' }));

  app.get('/plaintext', (req, res) =>
    res.header('Content-Type', 'text/plain').send('Hello, World!'));

  app.listen(8080);
}
