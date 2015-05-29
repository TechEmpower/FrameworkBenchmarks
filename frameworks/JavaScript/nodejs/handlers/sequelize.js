var h = require('../helper');
var async = require('async');
var Sequelize = require('sequelize');
var sequelize = new Sequelize('hello_world', 'benchmarkdbuser', 'benchmarkdbpass', {
  host: '127.0.0.1',
  dialect: 'mysql',
  logging: false
});

var Worlds = sequelize.define('World', {
  id:           { type: 'Sequelize.INTEGER' },
  randomNumber: { type: 'Sequelize.INTEGER' }
}, {
  timestamps: false,
  freezeTableName: true
});

var Fortunes = sequelize.define('Fortune', {
  id:      { type: 'Sequelize.INTEGER' },
  message: { type: 'Sequelize.STRING' }
}, {
  timestamps: false,
  freezeTableName: true
});

// Sequelize Query Functions
function sequelizeRandomWorld(callback) {
  Worlds.findOne({
    where: { id: h.randomTfbNumber() }
  }).complete(callback);
}

module.exports = {

  SingleQuery: function (req, res) {
    sequelizeRandomWorld(function (err, result) {
      if (err) { throw err; }
      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(result));
    });
  },

  MultipleQueries: function (queries, req, res) {
    var queryFunctions = h.fillArray(sequelizeRandomWorld, queries);

    async.parallel(queryFunctions, function (err, results) {
      if (err) { throw err; }
      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  },

  Fortunes: function (req, res) {
    Fortunes.findAll().complete(function (err, fortunes) {
      fortunes.push(h.ADDITIONAL_FORTUNE);
      fortunes.sort(function (a, b) {
        return a.message.localeCompare(b.message);
      });
      h.addTfbHeaders(res, 'html');
      res.end(h.fortunesTemplate({
        fortunes: fortunes
      }));
    });
  },

  Updates: function (queries, req, res) {
    var selectFunctions = h.fillArray(sequelizeRandomWorld, queries);

    async.parallel(selectFunctions, function (err, worlds) {
      if (err) { throw err; }
      var updateFunctions = [];

      for (var i = 0; i < queries; i++) {
        (function (i) {
          updateFunctions.push(function (callback) {
            worlds[i].randomNumber = h.randomTfbNumber();
            worlds[i].save().complete(callback);
          });
        })(i);
      }

      async.parallel(updateFunctions, function (err, updates) {
        if (err) { throw err; }
        h.addTfbHeaders(res, 'json');
        res.end(JSON.stringify(updates));
      });
    });
  }

}