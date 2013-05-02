var cluster = require('cluster')
  , numCPUs = require('os').cpus().length;

if(cluster.isMaster) {
  // Fork workers.
  for (var i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', function(worker, code, signal) {
    console.log('worker ' + worker.pid + ' died');
  });

  return;
}

var http = require('http')
  , url = require('url')
  , async = require('async')
  , mongoose = require('mongoose')
  , conn = mongoose.connect('mongodb://localhost/hello_world')
  , MongoClient = require('mongodb').MongoClient
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

var collection = null;

MongoClient.connect('mongodb://localhost/hello_world?maxPoolSize=5', function(err, db) {
  collection = db.collection('world');
});

// define model
var Schema = mongoose.Schema
  , ObjectId = Schema.ObjectId;

var WorldSchema = new Schema({
    id                           : Number
  , randomNumber                 : Number
}, { collection : 'world' });
var MWorld = conn.model('World', WorldSchema);



function getRandomNumber() {
  return Math.floor(Math.random() * 10000) + 1;
}

function mongooseQuery(callback) {
  MWorld.findOne({ id: getRandomNumber()}).exec(function (err, world) {
    callback(err, world);
  });
}

function mongodbDriverQuery(callback) {
  collection.findOne({ id: getRandomNumber()}, function(err, world) {
    callback(err, world);
  });
}

function sequelizeQuery(callback) {
  World.find(getRandomNumber()).success(function (world) {
    callback(null, world);
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

  case '/mongodbdriver':
    // Database Test
    var values = url.parse(req.url, true);
    var queries = values.query.queries || 1;
    var queryFunctions = new Array(queries);

    for (var i = 0; i < queries; i += 1) {
      queryFunctions[i] = mongodbDriverQuery;
    }

    res.writeHead(200, {'Content-Type': 'application/json; charset=UTF-8'});

    async.parallel(queryFunctions, function(err, results) {
      res.end(JSON.stringify(results));
    });
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
    var values = url.parse(req.url, true);
    var queries = values.query.queries || 1;
    var queryFunctions = new Array(queries);

    for (var i = 0; i < queries; i += 1) {
      queryFunctions[i] = sequelizeQuery;
    }

    res.writeHead(200, {'Content-Type': 'application/json'});

    async.parallel(queryFunctions, function(err, results) {
      res.end(JSON.stringify(results));
    });
    break;

  case '/mysql':
    res.writeHead(200, {'Content-Type': 'application/json'});

    function mysqlQuery(callback) {
      pool.getConnection(function(err, connection) {
        if (err) callback(err);
        connection.query("SELECT * FROM world WHERE id = " + getRandomNumber(), function(err, rows) {
          callback(null, rows[0]);
          connection.end();
        });
      });
    }

    var values = url.parse(req.url, true);
    var queries = values.query.queries || 1;
    var queryFunctions = new Array(queries);

    for (var i = 0; i < queries; i += 1) {
      queryFunctions[i] = mysqlQuery;
    }
    async.parallel(queryFunctions, function(err, results) {
      if (err) {
        res.writeHead(500);
        return res.end('MYSQL CONNECTION ERROR.');
      }
      res.end(JSON.stringify(results));
    });
    break;

  default:
    // File not found handler
    res.writeHead(404, {'Content-Type': 'text/html; charset=UTF-8'});
    res.end("NOT IMPLEMENTED");
  }
}).listen(8080);
