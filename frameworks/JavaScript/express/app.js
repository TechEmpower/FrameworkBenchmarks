
/**
 * Module dependencies.
 */

var cluster = require('cluster')
  , numCPUs = require('os').cpus().length
  , express = require('express')
  , Sequelize = require('sequelize')
  , mongoose = require('mongoose')
  , conn = mongoose.connect('mongodb://localhost/hello_world')
  , async = require('async');

// Middleware
var bodyParser = require('body-parser')
  , methodOverride = require('method-override')
  , errorHandler = require('errorhandler');

var Schema = mongoose.Schema
  , ObjectId = Schema.ObjectId;

var WorldSchema = new mongoose.Schema({
    id          : Number,
    randomNumber: Number
  }, {
    collection: 'world'
  }),
  MWorld = conn.model('World', WorldSchema);

var sequelize = new Sequelize('hello_world', 'benchmarkdbuser', 'benchmarkdbpass', {
  host: 'localhost',
  dialect: 'mysql',
  logging: false
});

var World = sequelize.define('World', {
  id: {
    type: 'Sequelize.INTEGER'
  },
  randomNumber: {
    type: 'Sequelize.INTEGER'
  }
}, {
  timestamps: false,
  freezeTableName: true
});
var Fortune = sequelize.define('Fortune', {
  id: {
    type: 'Sequelize.INTEGER'
  },
  message: {
    type: 'Sequelize.STRING'
  }
}, {
  timestamps: false,
  freezeTableName: true
});

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
  // https://github.com/expressjs/method-override#custom-logic
  app.use(bodyParser.urlencoded({extended: true}));
  app.use(methodOverride(function(req, res){
    if (req.body && typeof req.body === 'object' && '_method' in req.body) {
      // look in urlencoded POST bodies and delete it
      var method = req.body._method
      delete req.body._method
      return method
    }
  }));

  // Set headers for all routes
  app.use(function(req, res, next) {
    res.setHeader("Server", "Express");
    return next();
  });

  app.set('view engine', 'jade');
  app.set('views', __dirname + '/views');

  // Check Node env.
  var env = process.env.NODE_ENV || 'development';
  if ('development' == env) {
    app.use(errorHandler({ dumpExceptions: true, showStack: true }));
  }
  if ('production' == env) {
    app.use(errorHandler());
  }

  // Routes

  app.get('/json', function(req, res) {
    res.send({ message: 'Hello, World!' });
  });

  app.get('/plaintext', function(req, res) {
    res.header('Content-Type', 'text/plain').send('Hello, World!');
  });
  
  app.get('/mongoose', function(req, res) {
    var queries = isNaN(req.query.queries) ? 1 : parseInt(req.query.queries, 10)
      , queryFunctions = [];

    queries = Math.min(Math.max(queries, 1), 500);

    for (var i = 1; i <= queries; i++ ) {
      queryFunctions.push(function(callback) {
        MWorld.findOne({ id: (Math.floor(Math.random() * 10000) + 1) }).exec(callback);
      });
    }

    async.parallel(queryFunctions, function(err, results) {
      if (!req.query.queries) {
        results = results[0];
      }
      res.send(results);
    });
  });

  app.get('/mysql-orm', function(req, res) {    
    var queries = isNaN(req.query.queries) ? 1 : parseInt(req.query.queries, 10)
      , queryFunctions = [];

    queries = Math.min(Math.max(queries, 1), 500);

    for (var i = 1; i <= queries; i++ ) {
      queryFunctions.push(function(callback) {
        World.findOne({
          where: {
            id: Math.floor(Math.random() * 10000) + 1}
          }
        ).complete(callback);
      });
    }

    async.parallel(queryFunctions, function(err, results) {
      if (!req.query.queries) {
        results = results[0];
      }
      res.setHeader("Content-Type", "application/json");
      res.send(results);
    });
  });

  app.get('/fortune', function(req, res) {
    Fortune.findAll().complete(function (err, fortunes) {
      var newFortune = {id: 0, message: "Additional fortune added at request time."};
      fortunes.push(newFortune);
      fortunes.sort(function (a, b) {
        return (a.message < b.message) ? -1 : 1;
      });

      res.render('fortunes', {fortunes: fortunes});
    });
  });

  app.get('/mongoose-update', function(req, res) {
    var queries = isNaN(req.query.queries) ? 1 : parseInt(req.query.queries, 10)
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
    var queries = isNaN(req.query.queries) ? 1 : parseInt(req.query.queries, 10)
      , selectFunctions = [];

    queries = Math.max(Math.min(queries, 500), 1);

    for (var i = 1; i <= queries; i++ ) {
      selectFunctions.push(function(callback) {
        World.findOne({
          where: {
            id: Math.floor(Math.random() * 10000) + 1}
          }
        ).complete(callback);
      });
    }

    async.parallel(selectFunctions, function(err, worlds) {
      var updateFunctions = [];

      for (var i = 0; i < queries; i++) {
        (function(i){
          updateFunctions.push(function(callback){
            worlds[i].randomNumber = Math.ceil(Math.random() * 10000);
            worlds[i].save().complete(callback);
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
