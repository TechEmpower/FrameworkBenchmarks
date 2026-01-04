const cluster = require('cluster')
const numCPUs = require('os').cpus().length
const express = require('express');
const app = express();
const port = 8080;

if (cluster.isPrimary) {
  // Fork workers.
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) =>
    console.log('worker ' + worker.pid + ' died'));
} else {
  app.set('x-powered-by', false);
  app.set('etag', false)

  const resolvers = require('./resolver-postgres');

  // Routes
  require('./routes')(app, resolvers);

  app.listen(port, () => {
    console.log(`Listening on localhost:${port}`);
  });
}
