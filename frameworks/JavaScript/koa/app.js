var cluster = require('cluster')
  , numCPUs = require('os').cpus().length;

// Koa Deps
var koa = require('koa')
  , route = require('koa-route')
  , handlebars = require('koa-handlebars')
  , bodyParser = require('koa-bodyparser')
  , override = require('koa-override');

// Monk MongoDB Driver Deps
var monk = require('monk')
  , wrap = require('co-monk')
  , db = monk('mongodb://localhost/hello_world')
  , worlds = wrap(db.get('world'))
  , fortunes = wrap(db.get('fortune'));

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
  app.use(handlebars({
    // needed, otherwise missing dir err
    partialsDir: "views"
  }));

  // routes
  app.use(route.get('/json', jsonHandler));
  app.use(route.get('/db', dbHandler));
  app.use(route.get('/queries', queriesHandler));
  app.use(route.get('/fortunes', fortuneHandler));
  app.use(route.get('/updates', updateHandler));
  app.use(route.get('/plaintext', textHandler));

  // Helpers
  function getRandomNumber() {
    return Math.floor(Math.random()*10000) + 1;
  };

  function validateParam(param) {
    var numOfQueries = isNaN(param) ? 1 : param;
    if (numOfQueries > 500) {
      numOfQueries = 500;
    } else if (numOfQueries < 1) {
      numOfQueries = 1;
    }
    return numOfQueries;
  }

  // Query Helpers
  function *worldUpdateQuery() {
    var randomId = getRandomNumber();
    var randomNumber = getRandomNumber();
    var result = yield worlds.update(
      {id: randomId}, 
      {$set: {randomNumber: randomNumber}}
    );
    return {
      id: randomId,
      randomNumber: randomNumber
    }
  }

  function *worldQuery() {
    return yield worlds.findOne({id: getRandomNumber()}, '-_id');
  }

  function *fortunesQuery() {
    return yield fortunes.find({}, '-_id');
  }

  // Route handlers
  function *jsonHandler() {
    this.set('Server', 'Koa');
    this.body = {
      message: "Hello, world!"
    }
  }

  function *dbHandler() {
    this.set('Server', 'Koa');
    this.body = yield worldQuery;
  }

  function *queriesHandler() {
    this.set('Server', 'Koa');
    var numOfQueries = validateParam(this.query.queries);
    var queries = [];
    for (var i = 0; i < numOfQueries; i++) {
      queries.push(worldQuery);
    }
    this.body = yield queries;
  }

  function *fortuneHandler() {
    this.set('Server', 'Koa');
    var fortunes = yield fortunesQuery;
    fortunes.push({
      id: 0,
      message: 'Additional fortune added at request time.'
    });
    fortunes.sort(function(a, b) {
      return a.message < b.message ? -1 : 1;
    });
    yield this.render("fortunes", {fortunes: fortunes});
  }

  function *updateHandler() {
    this.set('Server', 'Koa');
    var numOfUpdates = validateParam(this.query.queries);
    var queries = [];
    for (var i = 0; i < numOfUpdates; i++) {
      queries.push(worldUpdateQuery);
    }
    this.body = yield queries;
  }

  function *textHandler() {
    this.set('Server', 'Koa');
    this.body = 'Hello, world!';
  }

  app.listen(8080);
}