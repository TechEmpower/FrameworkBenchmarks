const cluster = require('cluster')
const numCPUs = require('os').cpus().length
const express = require('ultimate-express');
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
  app.set("etag", false);
  app.use(express.urlencoded({ extended:false }));
  app.use(express.json());

  const resolvers = require('./resolver');

  // Routes
  require('./routes')(app, resolvers);

  app.listen(port, () => {
    console.log(`Listening on localhost:${port}`);
  });
}
