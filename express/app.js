
/**
 * Module dependencies.
 */

var cluster = require('cluster')
  , numCPUs = require('os').cpus().length
  , windows = require('os').platform() == 'win32'
  , express = require('express')
  , mongoose = require('mongoose')
  , async = require('async')
  , conn = mongoose.connect('mongodb://localhost/hello_world')
  , connMap = { user: 'benchmarkdbuser', password: 'benchmarkdbpass', database: 'hello_world', host: 'localhost' };

var Schema = mongoose.Schema
  , ObjectId = Schema.ObjectId;

var WorldSchema = new Schema({
    id                           : Number
  , randomNumber                 : Number
}, { collection : 'world' });
var MWorld = conn.model('World', WorldSchema);

if (!windows) {
  var Mapper = require('mapper');
  Mapper.connect(connMap, {verbose: false, strict: false});
  var World = Mapper.map("World", "id", "randomNumber");
  var Fortune = Mapper.map("Fortune", "id", "message");
}

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
    res.send({ message: 'Hello, World!' })
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
      if (queries == 1) {
        worlds = worlds[0];
      }
      res.send(worlds);
    });
  });

  app.get('/mysql-orm', function(req, res) {
    if (windows) return res.send(501, 'Not supported on windows');
    
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
    if (windows) return res.send(501, 'Not supported on windows');
    
    Fortune.all(function (err, fortunes) {
      var newFortune = {id: 0, message: "Additional fortune added at request time."};
      fortunes.push(newFortune);
      fortunes.sort(sortFortunes);

      res.render('fortunes', {fortunes: fortunes});
    });
  });

  function sortFortunes(a, b) {
    return (a.message < b.message) ? -1 : (a.message > b.message) ? 1 : 0;
  }

  app.get('/mongoose-update', function(req, res) {
    var queries = req.query.queries || 1
      , selectFunctions = [];

    queries = Math.min(queries, 500);

    for (var i = 1; i <= queries; i++ ) {
      selectFunctions.push(function(callback) {
        MWorld.findOne({ id: Math.floor(Math.random() * 10000) + 1 }).exec(callback);
      });
    }

    async.parallel(selectFunctions, function(err, worlds) {
      var updateFunctions = [];

      for (var i = 0; i < queries; i++) {
        (function(i){
          updateFunctions.push(function(callback){
            worlds[i].randomNumber = Math.ceil(Math.random() * 10000);
            MWorld.update({
              id: worlds[i]
            }, {
              randomNumber: worlds[i].randomNumber
            }, callback);
          });
        })(i);
      }

      async.parallel(updateFunctions, function(err, updates) {
        res.send(worlds);
      });
    });
  });

  app.get('/mysql-orm-update', function(req, res) {
    if (windows) return res.send(501, 'Not supported on windows');

    var queries = req.query.queries || 1
      , selectFunctions = [];

    queries = Math.min(queries, 500);

    for (var i = 1; i <= queries; i++ ) {
      selectFunctions.push(function(callback) {
        World.findById(Math.floor(Math.random() * 10000) + 1, callback);
      });
    }

    async.parallel(selectFunctions, function(err, worlds) {
      var updateFunctions = [];

      for (var i = 0; i < queries; i++) {
        (function(i){
          updateFunctions.push(function(callback){
            worlds[i].randomNumber = Math.ceil(Math.random() * 10000);
            World.save(worlds[i], callback);
          });
        })(i);
      }

      async.parallel(updateFunctions, function(err, updates) {
        res.send(worlds);
      });
    });   
  });

  app.listen(8080);
}
