var cluster = require('cluster')
  , numCPUs = require('os').cpus().length
  , http = require('http')
  , url = require('url')
  , async = require('async')
  , mongoose = require('mongoose')
  , conn = mongoose.connect('mongodb://localhost/hello_world')
  , mysql = require('mysql')
  , pool  = mysql.createPool({
      host: 'localhost',
      user     : 'benchmarkdbuser',
      password : 'benchmarkdbpass',
      database : 'hello_world',
      connectionLimit : 256
    })
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
  });

// define model
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

  return;
}

function mongooseQuery(callback) {
  MWorld.findOne({ id: (Math.floor(Math.random() * 10000) + 1)}).exec(function (err, world) {
    callback(err, world);
  });
}

  http.createServer(function (req, res) {
    // JSON response object
    var hello = {message: "Hello, world"};

    var path = url.parse(req.url).pathname;

    switch (path) {
    case '/json':
      // JSON Response Test
      res.writeHead(200, {'Content-Type': 'application/json; charset=UTF-8'});
      // Write JSON object to response
      res.end(JSON.stringify(hello));
      break;

    case '/mongoose':
      // Database Test
      var values = url.parse(req.url, true);
      var queries = values.query.queries || 1;

      var queryFunctions = new Array(queries);

      for (var i = 0; i < queries; i += 1) {
        queryFunctions[i] = mongooseQuery;
      }

      res.writeHead(200, {'Content-Type': 'application/json; charset=UTF-8'});

      async.parallel(queryFunctions, function(err, results) {
        res.end(JSON.stringify(results));
      });
      break;

    case '/sequelize':
        var queries = 1,
        worlds  = [],
        queryFunctions = [],
        values = url.parse(req.url, true);

      if ( values.query.queries ) {
        queries = values.query.queries;
      }

      res.writeHead(200, {'Content-Type': 'application/json'});

      for (var i = 1; i <= queries; i++ ) {
        queryFunctions.push(function(callback) {
          World.find(Math.floor(Math.random()*10000) + 1).success(function(world) {
            worlds.push(world);
            callback(null, 'success');
          });
        });
      }

      async.parallel(queryFunctions, function(err, results) {
        res.end(JSON.stringify(worlds));
      });
      break;

    case '/mysql':
        var queries = 1,
        worlds  = [],
        queryFunctions = [],
        values = url.parse(req.url, true);

      if ( values.query.queries ) {
        queries = values.query.queries;
      }

      res.writeHead(200, {'Content-Type': 'application/json'});
      
      pool.getConnection(function(err, connection) {
        for (var i = 1; i <= queries; i++ ) {
          queryFunctions.push(function(callback) {
            connection.query("SELECT * FROM World WHERE id = " + (Math.floor(Math.random()*10000) + 1), function(err, rows) {
              worlds.push(rows[0]);
              callback(null, 'success');
            });
          });
        }

        async.parallel(queryFunctions, function(err, results) {
          res.end(JSON.stringify(worlds));
          connection.end();
        });
      });
      break;

    default:
      // File not found handler
      res.writeHead(404, {'Content-Type': 'text/html; charset=UTF-8'});
      res.end("NOT IMPLEMENTED");
    }
  }).listen(8080);

