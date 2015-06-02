var h = require('../helper');
var async = require('async');
var mysql = require('mysql');
var connection = mysql.createConnection({
  host     : '127.0.0.1',
  user     : 'benchmarkdbuser',
  password : 'benchmarkdbpass',
  database : 'hello_world'
});

connection.connect();

var queries = {
  RANDOM_WORLD: "SELECT * FROM world WHERE id = " + h.randomTfbNumber(),
  ALL_FORTUNES: "SELECT * FROM fortune",
  UPDATE_WORLD: function (rows) {
    return [
      "UPDATE world SET randomNumber = ", rows[0].randomNumber,
      " WHERE id = ", rows[0]['id']
    ].join('');
  }
}

function mysqlRandomWorld(callback) {
  connection.query(queries.RANDOM_WORLD, function (err, rows, fields) {
    callback(err, rows[0]);
  });
}

function mysqlGetAllFortunes(callback) {
  connection.query(queries.ALL_FORTUNES, function (err, rows, fields) {
    callback(err, rows);
  })
}

function mysqlUpdateQuery(callback) {
  connection.query(queries.RANDOM_WORLD, function (err, rows, fields) {
    if (err) { return process.exit(1); }

    rows[0].randomNumber = h.randomTfbNumber();
    var updateQuery = queries.UPDATE_WORLD(rows);

    connection.query(updateQuery, function (err, result) {
      callback(err, rows[0]);
    });
  });
}

module.exports = {

  SingleQuery: function (req, res) {
    mysqlRandomWorld(function (err, result) {
      if (err) { return process.exit(1); }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(result));
    });
  },

  MultipleQueries: function (queries, req, res) {
    var queryFunctions = h.fillArray(mysqlRandomWorld, queries);

    async.parallel(queryFunctions, function (err, results) {
      if (err) { return process.exit(1); }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  },

  Fortunes: function (req, res) {
    mysqlGetAllFortunes(function (err, fortunes) {
      if (err) { return process.exit(1); }

      fortunes.push(h.ADDITIONAL_FORTUNE);
      fortunes.sort(function (a, b) {
        return a.message.localeCompare(b.message);
      })
      h.addTfbHeaders(res, 'html');
      res.end(h.fortunesTemplate({
        fortunes: fortunes
      }));
    });
  },

  Updates: function (queries, req, res) {
    var queryFunctions = h.fillArray(mysqlUpdateQuery, queries);

    async.parallel(queryFunctions, function (err, results) {
      if (err) { return process.exit(1); }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  } 

}