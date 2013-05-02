
/**
 * Module dependencies.
 */

var cluster = require('cluster')
  , numCPUs = require('os').cpus().length
  , express = require('express')
  , mongoose = require('mongoose')
  , async = require('async')
  , conn = mongoose.connect('mongodb://localhost/hello_world')
  , Sequelize = require("sequelize")
  , sequelize = new Sequelize('hello_world', 'benchmarkdbuser', 'benchmarkdbpass', {
    host: 'localhost',
    logging: false,
    define: { timestamps: false },
    maxConcurrentQueries: 100,
    pool: { maxConnections: 800, maxIdleTime: 30 }
  })
  , World      = sequelize.define('World', {
    randomNumber: Sequelize.INTEGER
  }, {
    freezeTableName: true
  })
  , Fortune      = sequelize.define('Fortune', {
    message: Sequelize.STRING
  }, {
    freezeTableName: true
  });

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

  app.get('/sequelize', function(req, res) {
    var queries = req.query.queries || 1
      , worlds  = []
      , queryFunctions = [];

    for (var i = 1; i <= queries; i++ ) {
      queryFunctions.push(function(callback) {
        World.find(Math.floor(Math.random()*10000) + 1).success(function (world) {
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
    Fortune.findAll().success(function (fortunes) {
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
