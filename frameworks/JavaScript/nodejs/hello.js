/**
 * Module dependencies.
 */

var cluster = require('cluster')
  , numCPUs = require('os').cpus().length
  , http = require('http')
  , url = require('url')
  , Sequelize = require('sequelize')
  , mysql = require('mysql')
  , async = require('async')
  , mongoose = require('mongoose')
  , conn = mongoose.connect('mongodb://127.0.0.1/hello_world')
  , MongoClient = require('mongodb').MongoClient;

var collection = null;
MongoClient.connect('mongodb://127.0.0.1/hello_world?maxPoolSize=5', function(err, db) {
  collection = db.collection('world');
});

var connection = mysql.createConnection({
  host     : '127.0.0.1',
  user     : 'benchmarkdbuser',
  password : 'benchmarkdbpass',
  database : 'hello_world'
});
connection.connect();

var WorldSchema = new mongoose.Schema({
    id          : Number,
    randomNumber: Number
  }, {
    collection: 'world'
  }),
  MWorld = conn.model('World', WorldSchema);

var sequelize = new Sequelize('hello_world', 'benchmarkdbuser', 'benchmarkdbpass', {
  host: '127.0.0.1',
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

// Mongoose Query Functions
function mongooseQuery(callback) {
  MWorld.findOne({
    id: getRandomNumber()
  }).exec(function (err, world) {
    callback(err, world);
  });
}

// MongoDB-Raw Query Functions
function mongodbDriverQuery(callback) {
  collection.findOne({
    id: getRandomNumber()
  }, function(err, world) {
    callback(err, world);
  });
}

function mongodbDriverUpdateQuery(callback) {
  collection.findAndModify({
    id: getRandomNumber()
  }, [['_id','asc']], {
    $set: {randomNumber: getRandomNumber()}
  }, {}, function(err, world) {
    callback(err, world && world.value);
  });
}

// Sequelize Query Functions
function sequelizeQuery(callback) {
  World.findOne({
    where: {
      id: Math.floor(Math.random() * 10000) + 1}
    }
  ).complete(callback);
}


// MySQL-Raw Query Functions
function mysqlQuery(callback) {
  connection.query("SELECT * FROM world WHERE id = " + getRandomNumber(), function (err, rows, fields) {
    if (err) {
      throw err;
    }
    callback(null, rows[0]);
  });
}

function mysqlUpdateQuery(callback) {
  connection.query("SELECT * FROM world WHERE id = " + getRandomNumber(), function (err, rows, fields) {
    if (err) {
      throw err;
    }
    rows[0].randomNumber = getRandomNumber();
    var updateQuery = "UPDATE World SET randomNumber = " + rows[0].randomNumber + " WHERE id = " + rows[0]['id'];
    connection.query(updateQuery, function (err, rows, field) {
      if (err) {
        throw err;
      }
      callback(null, rows[0]);
    });
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
      res.writeHead(200, {
        'Content-Type': 'text/plain; charset=UTF-8',
        'Server': 'Node'
      });
      res.end(helloStr);
      break;

    // Raw MongoDB Routes
    case '/mongodb':
      var values = url.parse(req.url, true);
      var queries = Math.min(Math.max(values.query.queries, 1), 500);
      var queryFunctions = [];

      queries = Math.min(Math.max(queries, 1), 500);

      for (var i = 0; i < queries; i += 1) {
        queryFunctions.push(mongodbDriverQuery);
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

    case '/mongodb-update':
      var values = url.parse(req.url, true);
      var queries = Math.min(Math.max(values.query.queries, 1), 500);
      var queryFunctions = [];

      queries = Math.min(Math.max(queries, 1), 500);

      for (var i = 0; i < queries; i += 1) {
        queryFunctions.push(mongodbDriverUpdateQuery);
      }

      async.parallel(queryFunctions, function(err, results) {
        res.writeHead(200, {
          'Content-Type': 'application/json',
          'Server': 'Node'
        });
        res.end(JSON.stringify(results));
      });
      break;

    // Mongoose ORM Routes
    case '/mongoose':
      var values = url.parse(req.url, true);
      var queries = isNaN(values.query.queries) ? 1 : parseInt(values.query.queries, 10);
      var queryFunctions = [];
      
      queries = Math.min(Math.max(queries, 1), 500);

      for (var i = 0; i < queries; i += 1) {
        queryFunctions.push(mongooseQuery);
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

    case '/mongoose-update':
      var values = url.parse(req.url, true);
      var queries = isNaN(values.query.queries) ? 1 : parseInt(values.query.queries, 10);
      var selectFunctions = [];
      
      queries = Math.min(Math.max(queries, 1), 500);

      for (var i = 0; i < queries; i += 1) {
        selectFunctions.push(mongooseQuery);
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
          res.writeHead(200, {
            'Content-Type': 'application/json',
            'Server': 'Node'
          });
          res.end(JSON.stringify(worlds));
        });
      });
      break;

    // Sequelize (MySQL ORM) Routes
    case '/mysql-orm':
      var values = url.parse(req.url, true);
      var queries = isNaN(values.query.queries) ? 1 : parseInt(values.query.queries, 10);
      var queryFunctions = [];

      queries = Math.min(Math.max(queries, 1), 500);

      for (var i = 0; i < queries; i += 1) {
        queryFunctions.push(sequelizeQuery);
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

    case '/mysql-orm-update':
      var values = url.parse(req.url, true);
      var queries = isNaN(values.query.queries) ? 1 : parseInt(values.query.queries, 10);
      var selectFunctions = [];

      queries = Math.min(Math.max(queries, 1), 500);

      for (var i = 0; i < queries; i += 1) {
        selectFunctions.push(sequelizeQuery);
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
          res.writeHead(200, {
            'Content-Type': 'application/json',
            'Server': 'Node'
          });
          res.end(JSON.stringify(worlds));
        });
      });
      break;

    // Raw MongoDB Routes
    case '/mysql':
      var values = url.parse(req.url, true);
      var queries = isNaN(values.query.queries) ? 1 : parseInt(values.query.queries, 10);
      var queryFunctions = [];

      queries = Math.min(Math.max(queries, 1), 500);

      for (var i = 0; i < queries; i += 1) {
        queryFunctions.push(mysqlQuery);
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

    case '/mysql-update':
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


    default:
      // File not found handler
      res.writeHead(501, {'Content-Type': 'text/plain; charset=UTF-8'});
      res.end("NOT IMPLEMENTED");
    }
  }).listen(8080);
}
