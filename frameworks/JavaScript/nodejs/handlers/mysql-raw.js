const h = require('../helper');
const async = require('async');
const mysql = require('mysql');
const connection = mysql.createConnection({
  host     : 'TFB-database',
  user     : 'benchmarkdbuser',
  password : 'benchmarkdbpass',
  database : 'hello_world'
});

connection.connect();

const queries = {
  RANDOM_WORLD: "SELECT * FROM world WHERE id = " + h.randomTfbNumber(),
  ALL_FORTUNES: "SELECT * FROM fortune",
  UPDATE_WORLD: (rows) => {
    return [
      "UPDATE world SET randomNumber = ", rows[0].randomNumber,
      " WHERE id = ", rows[0]['id']
    ].join('');
  }
}

const mysqlRandomWorld = (callback) => {
  connection.query(queries.RANDOM_WORLD, (err, rows, fields) => {
    callback(err, rows[0]);
  });
}

const mysqlGetAllFortunes = (callback) => {
  connection.query(queries.ALL_FORTUNES, (err, rows, fields) => {
    callback(err, rows);
  })
}

const mysqlUpdateQuery = (callback) => {
  connection.query(queries.RANDOM_WORLD, (err, rows, fields) => {
    if (err) { return process.exit(1); }

    rows[0].randomNumber = h.randomTfbNumber();
    const updateQuery = queries.UPDATE_WORLD(rows);

    connection.query(updateQuery, (err, result) => {
      callback(err, rows[0]);
    });
  });
}

module.exports = {

  SingleQuery: (req, res) => {
    mysqlRandomWorld((err, result) => {
      if (err) { return process.exit(1); }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(result));
    });
  },

  MultipleQueries: (queries, req, res) => {
    const queryFunctions = h.fillArray(mysqlRandomWorld, queries);

    async.parallel(queryFunctions, (err, results) => {
      if (err) { return process.exit(1); }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  },

  Fortunes: (req, res) => {
    mysqlGetAllFortunes((err, fortunes) => {
      if (err) { return process.exit(1); }

      fortunes.push(h.ADDITIONAL_FORTUNE);
      fortunes.sort((a, b) => {
        return a.message.localeCompare(b.message);
      })
      h.addTfbHeaders(res, 'html');
      res.end(h.fortunesTemplate({
        fortunes: fortunes
      }));
    });
  },

  Updates: (queries, req, res) => {
    const queryFunctions = h.fillArray(mysqlUpdateQuery, queries);

    async.parallel(queryFunctions, (err, results) => {
      if (err) { return process.exit(1); }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  } 

}
