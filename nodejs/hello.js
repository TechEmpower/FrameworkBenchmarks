var cluster = require('cluster')
  , numCPUs = require('os').cpus().length
  , windows = require('os').platform() == 'win32';

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
  , connMap = { user: 'benchmarkdbuser', password: 'benchmarkdbpass', database: 'hello_world', host: 'localhost' };

if (!windows) {
  var Mapper = require('mapper')  
    , libmysql = require('mysql-libmysqlclient').createConnectionSync();
    
    Mapper.connect(connMap, {verbose: false, strict: false});
    var World = Mapper.map("World", "id", "randomNumber")
    libmysql.connectSync('localhost', 'benchmarkdbuser', 'benchmarkdbpass', 'hello_world');
}

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

function mongodbDriverUpdateQuery(callback) {
  collection.findAndModify({ id: getRandomNumber()}, [['_id','asc']], {$set: {randomNumber: getRandomNumber()}}, {}, function(err, world) {
    callback(err, world);
  });
}

function sequelizeQuery(callback) {
  World.findById(getRandomNumber(), function (err, world) {
    callback(null, world);
  });
}

http.createServer(function (req, res) {
  // JSON response object
  var hello = {message: "Hello, World!"};
  var helloStr = "Hello, World!";
  var path = url.parse(req.url).pathname;
  
  // mysql on windows is not supported
  if (windows && (path.substr(0, 3) == '/my' || path == '/update')) {
    path = '/doesntexist';
  }

  switch (path) {
  case '/json':
    // JSON Response Test
    res.writeHead(200, {'Content-Type': 'application/json; charset=UTF-8'});
    // Write JSON object to response
    res.end(JSON.stringify(hello));
    break;

  case '/plaintext':
    // JSON Response Test
    res.writeHead(200, {'Content-Type': 'text/plain; charset=UTF-8'});
    // Write JSON object to response
    res.end(helloStr);
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

  case '/mysql-orm':
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

    function libmysqlQuery(callback) {
      libmysql.query("SELECT * FROM world WHERE id = " + getRandomNumber(), function (err, res) {
        if (err) {
	        throw err;
	      }
	
	      res.fetchAll(function(err, rows) {
      	  if (err) {
      	    throw err;
      	  }

      	  res.freeSync();
      	  callback(null, rows[0]);
        });
      });
    } 

    var values = url.parse(req.url, true);
    var queries = values.query.queries || 1;
    var queryFunctions = new Array(queries);

    for (var i = 0; i < queries; i += 1) {
      queryFunctions[i] = libmysqlQuery;
    }
    async.parallel(queryFunctions, function(err, results) {
      if (err) {
        res.writeHead(500);
        return res.end('MYSQL CONNECTION ERROR.');
      }
      res.end(JSON.stringify(results));
    });
    break;

  case '/update':
    res.writeHead(200, {'Content-Type': 'application/json'});

    function libmysqlQuery(callback) {
      libmysql.query("SELECT * FROM world WHERE id = " + getRandomNumber(), function (err, res) {
        if (err) {
          throw err;
        }
  
        res.fetchAll(function(err, rows) {
          if (err) {
            throw err;
          }

          res.freeSync();

          rows[0].randomNumber = getRandomNumber();
          libmysql.query("UPDATE World SET randomNumber = " + rows[0].randomNumber + " WHERE id = " + rows[0]['id'], function (err, res) {
            if (err) {
              throw err;
            }
            callback(null, rows[0]);
          });
        });
      });
    } 

    var values = url.parse(req.url, true);
    var queries = values.query.queries || 1;
    if(queries < 1) {
      queries = 1;
    } else if(queries > 500) {
      queries = 500;
    }
    var queryFunctions = new Array(queries);

    for (var i = 0; i < queries; i += 1) {
      queryFunctions[i] = libmysqlQuery;
    }
    async.parallel(queryFunctions, function(err, results) {
      if (err) {
        res.writeHead(500);
        return res.end('MYSQL CONNECTION ERROR.');
      }
      res.end(JSON.stringify(results));
    });
    break;

  case '/update-mongodb':
    // Database Test
    var values = url.parse(req.url, true);
    var queries = values.query.queries || 1;
    if (queries < 1) {
      queries = 1;
    } else if (queries > 500) {
      queries = 500;
    }

    var queryFunctions = new Array(queries);

    for (var i = 0; i < queries; i += 1) {
      queryFunctions[i] = mongodbDriverUpdateQuery;
    }

    res.writeHead(200, {'Content-Type': 'application/json; charset=UTF-8'});

    async.parallel(queryFunctions, function(err, results) {
      res.end(JSON.stringify(results));
    });
    break;

  default:
    // File not found handler
    res.writeHead(501, {'Content-Type': 'text/plain; charset=UTF-8'});
    res.end("NOT IMPLEMENTED");
  }
}).listen(8080);
