/**
 * Module dependencies.
 */

var cluster = require('cluster')
  , numCPUs = require('os').cpus().length
  , http = require('http')
  , url = require('url')
  , Sequelize = require('sequelize')
  , async = require('async')
  , mongoose = require('mongoose')
  , conn = mongoose.connect('mongodb://localhost/hello_world')
  , MongoClient = require('mongodb').MongoClient;

var collection = null;
MongoClient.connect('mongodb://localhost/hello_world?maxPoolSize=5', function(err, db) {
  collection = db.collection('world');
});

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
  logging: false,
  pool: {
    max: 5000,
    min: 0,
    idle: 5000
  }
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

// Helper functions
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

if(cluster.isMaster) {
  // Fork workers.
  for (var i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', function(worker, code, signal) {
    console.log('worker ' + worker.pid + ' died');
  });

  return;
} else {
  http.createServer(function (req, res) {
    // JSON response object
    var hello = {message: "Hello, World!"};
    var helloStr = "Hello, World!";
    var path = url.parse(req.url).pathname;

    switch (path) {
    case '/json':
      res.writeHead(200, {
        'Content-Type': 'application/json', 
        'Server': 'Node'
      });
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
      var queries = Math.min(Math.max(values.query.queries, 1), 500);
      var queryFunctions = new Array(queries);

      for (var i = 0; i < queries; i += 1) {
        queryFunctions[i] = mongodbDriverQuery;
      }


      async.parallel(queryFunctions, function(err, results) {
        if (!values.query.queries) {
          results = results[0];
        }
        res.writeHead(200, {
          'Content-Type': 'application/json', 
          'Server': 'Node'
        });
        res.end(JSON.stringify(results));
      });
      break;

    case '/mongoose':
      // Database Test
      var values = url.parse(req.url, true);
      var queries = Math.min(Math.max(values.query.queries, 1), 500);
      var queryFunctions = new Array(queries);

      for (var i = 0; i < queries; i += 1) {
        queryFunctions[i] = mongooseQuery;
      }
      

      async.parallel(queryFunctions, function(err, results) {
        if (!values.query.queries) {
          results = results[0];
        }
        res.writeHead(200, {
          'Content-Type': 'application/json', 
          'Server': 'Node'
        });
        res.end(JSON.stringify(results));
      });
      break;

    case '/mysql-orm':
      var values = url.parse(req.url, true);
      var queries = Math.min(Math.max(values.query.queries, 1), 500);
      var queryFunctions = new Array(queries);

      for (var i = 0; i < queries; i += 1) {
        queryFunctions[i] = sequelizeQuery;
      }


      async.parallel(queryFunctions, function(err, results) {
        if (queries == 1) {
          results = results[0];
        }
        res.writeHead(200, {
          'Content-Type': 'application/json', 
          'Server': 'Node'
        });
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
        if (!values.query.queries) {
          results = results[0];
        }
        res.writeHead(200, {
          'Content-Type': 'application/json', 
          'Server': 'Node'
        });
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
        res.writeHead(200, {
          'Content-Type': 'application/json', 
          'Server': 'Node'
        });
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

      async.parallel(queryFunctions, function(err, results) {
        res.writeHead(200, {
          'Content-Type': 'application/json', 
          'Server': 'Node'
        });
        res.end(JSON.stringify(results));
      });
      break;

    default:
      // File not found handler
      res.writeHead(501, {'Content-Type': 'text/plain; charset=UTF-8'});
      res.end("NOT IMPLEMENTED");
    }
  }).listen(8080);
}
