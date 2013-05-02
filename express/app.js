
/**
 * Module dependencies.
 */

var cluster = require('cluster')
  , numCPUs = require('os').cpus().length
  , express = require('express')
  , mongoose = require('mongoose')
  , async = require('async')
  , conn = mongoose.connect('mongodb://localhost/hello_world')
  , Mapper = require('mapper')
  , connMap = { user: 'benchmarkdbuser', password: 'benchmarkdbpass', database: 'hello_world' };

var Schema = mongoose.Schema
  , ObjectId = Schema.ObjectId;

var WorldSchema = new Schema({
    id                           : Number
  , randomNumber                 : Number
}, { collection : 'world' });
var MWorld = conn.model('World', WorldSchema);

Mapper.connect(connMap, {verbose: false, strict: false});
var World = Mapper.map("World", "id", "randomNumber");
var Fortune = Mapper.map("Fortune", "id", "message");

if (cluster.isMaster) {
  // Fork workers.
  for (var i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', function(worker, code, signal) {
    console.log('worker ' + worker.pid + ' died');
  });
} else {
  var app = module.exports = express();

  // Configuration
  app.configure(function(){
    app.use(express.bodyParser());
    app.use(express.methodOverride());
    app.use(app.router);

    app.set('view engine', 'jade');
    app.set('views', __dirname + '/views');
  });

  app.configure('development', function() {
    app.use(express.errorHandler({ dumpExceptions: true, showStack: true }));
  });

  app.configure('production', function() {
    app.use(express.errorHandler());
  });

  // Routes

  app.get('/json', function(req, res) {
    res.send({ message: 'Hello World!' })
  });
  
  app.get('/mongoose', function(req, res) {
    var queries = req.query.queries || 1,
        worlds  = [],
        queryFunctions = [];

    for (var i = 1; i <= queries; i++ ) {
      queryFunctions.push(function(callback) {
        MWorld.findOne({ id: (Math.floor(Math.random() * 10000) + 1 )}).exec(function (err, world) {
          worlds.push(world);
          callback(null, 'success');
        });
      });
    }

    async.parallel(queryFunctions, function(err, results) {
      res.send(worlds);
    });
  });

  app.get('/mysql-orm', function(req, res) {
    var queries = req.query.queries || 1
      , worlds  = []
      , queryFunctions = [];

    for (var i = 1; i <= queries; i++ ) {
      queryFunctions.push(function(callback) {
        World.findById(Math.floor(Math.random()*10000) + 1, function (err, world) {
          worlds.push(world);
          callback(null, 'success');
        });
      });
    }

    async.parallel(queryFunctions, function(err, results) {
      res.send(worlds);
    });
  });

  app.get('/fortune', function(req, res) {
    Fortune.all(function (err, fortunes) {
      var newFortune = Fortune.build({message: "Additional fortune added at request time."});
      fortunes.push(newFortune);
      fortunes.sort(sortFortunes);

      res.render('fortunes', {fortunes: fortunes});
    });
  });

  function sortFortunes(a, b) {
    return (a.message < b.message) ? -1 : (a.message > b.message) ? 1 : 0;
  }

  app.listen(8080);
}
