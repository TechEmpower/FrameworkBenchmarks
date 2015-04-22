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
    callback(err, world && world.value);
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

  // mysql on windows is not supported
  if (windows && (req.url.substr(0, 3) == '/my' || req.url == '/update')) {
    req.url = '/doesntexist';
  }

  switch (req.url) {
  case '/json':
    // JSON Response Test
    res.writeHead(200, {'Content-Type': 'application/json; charset=UTF-8', 'Server': 'Node'});
    // Write JSON object to response
    res.end(JSON.stringify(hello));
    return;

  case '/plaintext':
    // JSON Response Test
    res.writeHead(200, {'Content-Type': 'text/plain; charset=UTF-8', 'Server': 'Node'});
    // Write JSON object to response
    res.end(helloStr);
    return;
  }

  var values = url.parse(req.url, true);
  var queries = Math.min(Math.max(+values.query.queries || 1, 1), 500);
  var queryFunctions = new Array(queries);

  switch (values.pathname) {
  case '/mongodbdriver':
    // Database Test
    for (var i = 0; i < queries; i += 1) {
      queryFunctions[i] = mongodbDriverQuery;
    }

    async.parallel(queryFunctions, function(err, results) {
      if (!values.query.queries) {
        results = results[0];
      }
      res.writeHead(200, {'Content-Type': 'application/json; charset=UTF-8', 'Server': 'Node'});
      res.end(JSON.stringify(results));
    });
    break;

  case '/mongoose':
    // Database Test
    for (var i = 0; i < queries; i += 1) {
      queryFunctions[i] = mongooseQuery;
    }

    async.parallel(queryFunctions, function(err, results) {
      if (!values.query.queries) {
        results = results[0];
      }
      res.writeHead(200, {'Content-Type': 'application/json; charset=UTF-8', 'Server': 'Node'});
      res.end(JSON.stringify(results));
    });
    break;

  case '/mysql-orm':
    for (var i = 0; i < queries; i += 1) {
      queryFunctions[i] = sequelizeQuery;
    }

    async.parallel(queryFunctions, function(err, results) {
      if (!values.query.queries) {
        results = results[0];
      }
      res.writeHead(200, {'Content-Type': 'application/json', 'Server': 'Node'});
      res.end(JSON.stringify(results));
    });
    break;

  case '/mysql':

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

    for (var i = 0; i < queries; i += 1) {
      queryFunctions[i] = libmysqlQuery;
    }
    async.parallel(queryFunctions, function(err, results) {
      if (err) {
        res.writeHead(500);
        return res.end('MYSQL CONNECTION ERROR.');
      }
      if (!values.query.queries) {
        results = results[0];
      }
      res.writeHead(200, {'Content-Type': 'application/json', 'Server': 'Node'});
      res.end(JSON.stringify(results));
    });
    break;

  case '/update':

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

    for (var i = 0; i < queries; i += 1) {
      queryFunctions[i] = libmysqlQuery;
    }
    async.parallel(queryFunctions, function(err, results) {
      if (err) {
        res.writeHead(500);
        return res.end('MYSQL CONNECTION ERROR.');
      }
      res.writeHead(200, {'Content-Type': 'application/json', 'Server': 'Node'});
      res.end(JSON.stringify(results));
    });
    break;

  case '/update-mongodb':
    // Database Test
    for (var i = 0; i < queries; i += 1) {
      queryFunctions[i] = mongodbDriverUpdateQuery;
    }

    async.parallel(queryFunctions, function(err, results) {
      res.writeHead(200, {'Content-Type': 'application/json; charset=UTF-8', 'Server': 'Node'});
      res.end(JSON.stringify(results));
    });
    break;

  default:
    // File not found handler
    res.writeHead(501, {'Content-Type': 'text/plain; charset=UTF-8'});
    res.end("NOT IMPLEMENTED");
  }
}).listen(8080);
