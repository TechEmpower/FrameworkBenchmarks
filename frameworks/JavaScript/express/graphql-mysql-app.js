const cluster = require('cluster')
const numCPUs = require('os').cpus().length
const express = require('express');
const app = express();
const bodyParser = require('body-parser');
const port = 8080;

if (cluster.isMaster) {
  // Fork workers.
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) =>
    console.log('worker ' + worker.pid + ' died'));
} else {
  app.use(bodyParser.urlencoded({ extended:false }));
  app.use(bodyParser.json());

  const resolvers = require('./resolver');

  // Routes
  require('./routes')(app, resolvers);

  app.listen(port, () => {
    console.log(`Listening on localhost:${port}`);
  });
}
