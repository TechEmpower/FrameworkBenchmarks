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
  MWorld.findOne({ id: getRandomNumber()}).exec(callback);
}

function mongodbDriverQuery(callback) {
  collection.findOne({ id: getRandomNumber()}, callback);
}

function sequelizeQuery(callback) {
  World.find(getRandomNumber()).success(function (world) {
    callback(null, world);
  });
}

function mysqlQuery(callback) {
  pool.getConnection(function(err, connection) {
    if (err) return callback(err);
    connection.query("SELECT * FROM world WHERE id = " + getRandomNumber(), function(err, rows) {
      callback(null, rows[0]);
      connection.end();
    });
  });
}

function streamJSON(req, res, fn) {
  var values = url.parse(req.url, true);
  var queries = parseInt(values.query.queries, 10) || 1;

  res.writeHead(200, {'Content-Type': 'application/json; charset=UTF-8'});

  res.write('[');

  for (var i = 0; i < queries; i++) fn(write);

  // On each callback,
  // write the JSON document immediately,
  // add a closing "]" if it's the last document,
  // add a trailing "," otherwise.
  var errored;

  function write(err, doc) {
    if (errored) return;

    if (err) {
      // We've already sent the headers,
      // so there's nothing we can really do
      // except finish up the JSON response.
      errored = true;
      res.end(']');
      return
    }

    res.write(JSON.stringify(doc));

    if (--queries) res.write(',');
    else res.end(']');
  }
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
    streamJSON(req, res, mongodbDriverQuery);
    break;

  case '/mongoose':
    streamJSON(req, res, mongooseQuery);
    break;

  case '/sequelize':
    streamJSON(req, res, sequelizeQuery);
    break;

  case '/mysql':
    streamJSON(req, res, mysqlQuery);
    break;

  default:
    // File not found handler
    res.writeHead(404, {'Content-Type': 'text/html; charset=UTF-8'});
    res.end("NOT IMPLEMENTED");
  }
}).listen(8080);
