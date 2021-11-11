const cluster = require('cluster')
const numCPUs = require('os').cpus().length
const express = require('express');
const mongoose = require('mongoose');
const app = express();
const bodyParser = require('body-parser');
const port = 8080;

mongoose.Promise = global.Promise;

mongoose.connect('mongodb://tfb-database/hello_world').then(() => {
  console.log('connected to mongo tfb-database hello_world');
}).catch((err) => {
  console.log('Failed connection attempt to Mongo: ', err);
});

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

  const resolvers = require('./resolver-mongo');

  // Routes
  require('/routes.js')(app, resolvers);

  app.listen(port, () => {
    console.log(`Listening on localhost:${port}`);
  });
}
