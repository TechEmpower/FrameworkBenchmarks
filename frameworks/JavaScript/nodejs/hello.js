var cluster = require('cluster')
  , numCPUs = require('os').cpus().length
  , http = require('http')
  , url = require('url')
  , Sequelize = require('sequelize')
  , mysql = require('mysql')
  , async = require('async')
  , mongoose = require('mongoose')
  , conn = mongoose.connect('mongodb://127.0.0.1/hello_world')
  , MongoClient = require('mongodb').MongoClient
  , Handlebars = require('handlebars');

// MongoDB Raw Setup
var collection = null;
MongoClient.connect('mongodb://127.0.0.1/hello_world?maxPoolSize=5', function(err, db) {
  collection = db.collection('world');
});

// MySQL Raw Setup
var connection = mysql.createConnection({
  host     : '127.0.0.1',
  user     : 'benchmarkdbuser',
  password : 'benchmarkdbpass',
  database : 'hello_world'
});
connection.connect();

// Mongoose Setup
var WorldSchema = new mongoose.Schema({
    id          : Number,
    randomNumber: Number
  }, {
    collection: 'world'
  }),
  MWorld = conn.model('World', WorldSchema);

// Sequelize Setup
var sequelize = new Sequelize('hello_world', 'benchmarkdbuser', 'benchmarkdbpass', {
  host: '127.0.0.1',
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

// Fortunes template via handlebars setup
var FORTUNES_TEMPLATE = [
  "<!DOCTYPE html>",
  "<html>",
  "<head><title>Fortunes</title></head>",
  "<body>",
  "<table>",
    "<tr>",
      "<th>id</th>",
      "<th>message</th>",
    "</tr>",
    "{{#fortunes}}",
    "<tr>",
      "<td>{{id}}</td>",
      "<td>{{message}}</td>",
    "</tr>",
    "{{/fortunes}}",
  "</table>",
  "</body>",
  "</html>"
].join('');

// Helper functions
function getRandomNumber() {
  return Math.floor(Math.random() * 10000) + 1;
}

function fillArray(value, len) {
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr.push(value);
  }
  return arr;
}

function addTfbHeaders(res, headerType) {
  var headers = {
    'Server': 'Node'
  }

  if (headerType === 'plain') {
    headers['Content-Type'] = 'text/plain; charset=UTF-8';
  } else if (headerType === 'json') {
    headers['Content-Type'] = 'application/json';
  } else if (headerType === 'html') {
    headers['Content-Type'] = 'text/html; cherset=UTF-8';
  }

  res.writeHead(200, headers);
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
    world._id = undefined; // remove _id from query response
    callback(err, world);
  });
}

function mongodbDriverUpdateQuery(callback) {
  collection.findAndModify({
    id: getRandomNumber()
  }, [['_id','asc']], {
    $set: {randomNumber: getRandomNumber()}
  }, {}, function(err, world) {
    world.value._id = undefined; // remove _id from query response
    callback(err, world.value);
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
    var updateQuery = "UPDATE world SET randomNumber = " + rows[0].randomNumber + " WHERE id = " + rows[0]['id'];
    connection.query(updateQuery, function (err, result) {
      if (err) {
        throw err;
      }
      callback(null, rows[0]);
    });
  });
}

var GREETING = "Hello, World!";
var HELLO_OBJ = { message: GREETING };
var ADDITIONAL_FORTUNE = {
  id: 0,
  message: 'Additional fortune added at request time.'
};

var responses = {

  jsonSerialization: function (req, res) {
    addTfbHeaders(res, 'json');
    res.end(JSON.stringify(HELLO_OBJ));
  },

  plaintext: function (req, res) {
    addTfbHeaders(res, 'plain');
    res.end(GREETING);
  },

  mongooseSingleQuery: function (req, res) {
    mongooseQuery(function (err, result) {
      if (err) { throw err; }
      addTfbHeaders(res, 'json');
      res.end(JSON.stringify(result));
    })
  },

  mongooseMultipleQueries: function (queries, req, res) {
    var queryFunctions = fillArray(mongooseQuery, queries)

    async.parallel(queryFunctions, function (err, results) {
      if (err) { throw err; }
      addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  },

  mongooseUpdates: function (queries, req, res) {
    var selectFunctions = fillArray(mongooseQuery, queries);

    async.parallel(selectFunctions, function (err, worlds) {
      var updateFunctions = [];

      for (var i = 0; i < queries; i++) {
        (function (i) {
          updateFunctions.push(function (callback) {
            worlds[i].randomNumber = getRandomNumber();
            MWorld.update({
              id: worlds[i].id
            }, {
              randomNumber: worlds[i].randomNumber
            }, callback);
          });
        })(i);
      }

      async.parallel(updateFunctions, function (err, results) {
        if (err) { throw err; }
        addTfbHeaders(res, 'json');
        // results does not have updated document information
        // if no err was found and thrown: all updates succeeded
        res.end(JSON.stringify(worlds));
      });
    });
  },

  mongodbSingleQuery: function (req, res) {
    mongodbDriverQuery(function (err, result) {
      if (err) { throw err; }
      addTfbHeaders(res, 'json');
      res.end(JSON.stringify(result));
    });
  },

  mongodbMultipleQueries: function (queries, req, res) {
    var queryFunctions = fillArray(mongodbDriverQuery, queries);

    async.parallel(queryFunctions, function (err, results) {
      if (err) { throw err; }
      addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  },

  mongodbUpdates: function (queries, req, res) {
    var queryFunctions = fillArray(mongodbDriverUpdateQuery, queries);

    async.parallel(queryFunctions, function (err, results) {
      if (err) { throw err; }
      addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  },

  sequelizeSingleQuery: function (req, res) {
    sequelizeQuery(function (err, result) {
      if (err) { throw err; }
      addTfbHeaders(res, 'json');
      res.end(JSON.stringify(result));
    });
  },

  sequelizeMultipleQueries: function (queries, req, res) {
    var queryFunctions = fillArray(sequelizeQuery, queries);

    async.parallel(queryFunctions, function (err, results) {
      if (err) { throw err; }
      addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  },

  sequelizeFortunes: function (req, res) {
    Fortune.findAll().complete(function (err, fortunes) {
      fortunes.push(ADDITIONAL_FORTUNE);
      fortunes.sort(function (a, b) {
        return a.message.localeCompare(b.message);
      });
      addTfbHeaders(res, 'html');
      var template = Handlebars.compile(FORTUNES_TEMPLATE);
      res.end(template({
        fortunes: fortunes
      }));
    });
  },

  sequelizeUpdates: function (queries, req, res) {
    var selectFunctions = fillArray(sequelizeQuery, queries);

    async.parallel(selectFunctions, function (err, worlds) {
      var updateFunctions = [];

      for (var i = 0; i < queries; i++) {
        (function (i) {
          updateFunctions.push(function (callback) {
            worlds[i].randomNumber = getRandomNumber();
            worlds[i].save().complete(callback);
          });
        })(i);
      }

      async.parallel(updateFunctions, function (err, updates) {
        if (err) { throw err; }
        addTfbHeaders(res, 'json');
        res.end(JSON.stringify(updates));
      });
    });
  },

  mysqlSingleQuery: function (req, res) {
    mysqlQuery(function (err, result) {
      if (err) { throw err; }
      addTfbHeaders(res, 'json');
      res.end(JSON.stringify(result));
    });
  },

  mysqlMultipleQueries: function (queries, req, res) {
    var queryFunctions = fillArray(mysqlQuery, queries);

    async.parallel(queryFunctions, function (err, results) {
      if (err) { throw err; }
      addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  },

  mysqlUpdates: function (queries, req, res) {
    var queryFunctions = fillArray(mysqlUpdateQuery, queries);

    async.parallel(queryFunctions, function (err, results) {
      if (err) { throw err; }
      addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  },

  routeNotImplemented: function (req, res) {
    res.writeHead(501, {'Content-Type': 'text/plain; charset=UTF-8'});
    var reason = { reason: "`" + req.url + "` is not an implemented route" };
    res.end(JSON.stringify(reason));
  }

};

if (cluster.isMaster) {
  // Fork workers.
  for (var i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', function (worker, code, signal) {
    console.log('worker ' + worker.pid + ' died');
  });
} else {
  http.createServer(function (req, res) {
    var values = url.parse(req.url, true);
    var route = values.pathname;

    // Basic routes, no db required
    if (route === '/json') {
      return responses.jsonSerialization(req, res);
    } else if (route === '/plaintext') {
      return responses.plaintext(req, res);

    // No queries parameter required
    } else if (route === '/mongoose/db') {
      return responses.mongooseSingleQuery(req, res);
    } else if (route === '/mongodb/db') {
      return responses.mongodbSingleQuery(req, res);
    } else if (route === '/mysql-orm/db') {
      return responses.sequelizeSingleQuery(req, res);
    } else if (route === '/mysql-orm/fortunes') {
      return responses.sequelizeFortunes(req, res);
    } else if (route === '/mysql/db') {
      return responses.mysqlSingleQuery(req, res);
    }

    else {
      var queries = isNaN(values.query.queries) ? 1 : parseInt(values.query.queries, 10);
      queries = Math.min(Math.max(queries, 1), 500);

      if (route === '/mongoose/queries') {
        return responses.mongooseMultipleQueries(queries, req, res);
      } else if (route === '/mongoose/updates') {
        return responses.mongooseUpdates(queries, req, res);
      } else if (route === '/mongodb/queries') {
        return responses.mongodbMultipleQueries(queries, req, res);
      } else if (route === '/mongodb/updates') {
        return responses.mongodbUpdates(queries, req, res);
      } else if (route === '/mysql-orm/queries') {
        return responses.sequelizeMultipleQueries(queries, req, res);
      } else if (route === '/mysql-orm/updates') {
        return responses.sequelizeUpdates(queries, req, res);
      } else if (route === '/mysql/queries') {
        return responses.mysqlMultipleQueries(queries, req, res);
      } else if (route === '/mysql/updates') {
        return responses.mysqlUpdates(queries, req, res);
      } else {
        return responses.routeNotImplemented(req, res);
      }
    }
  }).listen(8080, function() {
    console.log("NodeJS worker listening on port 8080");
  });
}
