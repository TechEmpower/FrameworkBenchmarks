
/**
 * Module dependencies.
 */

const cluster = require('cluster'),
  numCPUs = require('os').cpus().length,
  express = require('express');

const {
  jsonSerializer,
  GREETING,
} = require("./src/utils.mjs");


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

  app.set('x-powered-by', false);
  app.set('etag', false);

  // Routes
  app.get('/json', (req, res) => {
    res.writeHead(200, {
      "content-type": "application/json",
      server: "Express",
    }).end(jsonSerializer({ message: GREETING }))
  });

  app.get('/plaintext', (req, res) => {
    res.writeHead(200, {
      "content-type": "text/plain",
      server: "Express",
    }).end(GREETING);
  });

  const server = app.listen(8080);
  server.keepAliveTimeout = 0;
}
