var cluster = require('cluster')
  , numCPUs = require('os').cpus().length
  , koa = require('koa')
  , route = require('koa-route')
  , bodyParser = require('koa-bodyparser')
  , override = require('koa-override')
  , mongo = require('koa-mongo');

if (cluster.isMaster) {
  // Fork workers.
  for (var i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', function(worker, code, signal) {
    console.log('worker ' + worker.process.pid + ' died');
  });
} else {
  var app = module.exports = koa();
  app.use(bodyParser());
  app.use(override());
  app.use(mongo({
    uri: "mongodb://localhost/hello_world"
  }));

  // routes
  app.use(route.get('/json', jsonHandler));
  app.use(route.get('/db', dbHandler));
  app.use(route.get('/queries', queriesHandler));
  // app.use(route.get('/fortune', fortuneHandler));
  // app.use(route.get('/update', updateHandler));
  app.use(route.get('/plaintext', textHandler));

  function *worldQuery() {
    return yield function(callback) {
        var randomId = {id: Math.floor(Math.random()*10000) + 1};
        this.mongo.collection('world').findOne(randomId, {_id: 0}, callback);
    }
  }

  function *jsonHandler() {
    this.response.body = {
      message: "Hello, world!"
    }
  }

  function *dbHandler() {
    this.body = yield worldQuery;
  }

  function *queriesHandler() {
    var numOfQueries = this.query.queries || 1,
        queries = [];
    for (var i = 0; i < numOfQueries; i++) {
      queries.push(worldQuery);
    }
    this.body = yield queries;
  }

  function *textHandler() {
    this.body = 'Hello, world!'
  }
  app.listen(3000); //used for local testing
}