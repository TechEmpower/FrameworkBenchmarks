var cluster = require('cluster')
  , numCPUs = require('os').cpus().length
  , koa = require('koa')
  , route = require('koa-route')
  , bodyParser = require('koa-bodyparser')
  , override = require('koa-override')
  , mongoose = require('mongoose')
  , conn = mongoose.connect('mongodb://localhost/hello_world');

var Schema = mongoose.Schema
  , ObjectId = Schema.ObjectId;

var WorldSchema = new Schema({
    id                           : Number
  , randomNumber                 : Number
}, { collection : 'world' });
var MWorld = conn.model('World', WorldSchema);

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

  // routes
  app.use(route.get('/json', jsonHandler));
  app.use(route.get('/db', dbHandler));
  // app.use(route.get('/queries', queriesHandler));
  // app.use(route.get('/fortune', fortuneHandler));
  // app.use(route.get('/update', updateHandler));
  app.use(route.get('/plaintext', textHandler));

  function *jsonHandler() {
    this.response.body = {
      message: "Hello, world!"
    }
  }

  function *dbHandler() {
    var queries = this.request.queries.queries
  }

  function *textHandler() {
    this.body = 'Hello, world!'
  }
  app.listen(3000); //used for local testing
}