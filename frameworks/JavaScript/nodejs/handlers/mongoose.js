var h = require('../helper');
var async = require('async');
var Mongoose = require('mongoose');
var connection = Mongoose.connect('mongodb://127.0.0.1/hello_world')

// Mongoose Setup
var WorldSchema = new Mongoose.Schema({
    id          : Number,
    randomNumber: Number
  }, {
    collection: 'world'
  });
var FortuneSchema = new Mongoose.Schema({
    id:      Number,
    message: String
  }, {
    collection: 'fortune'
  });

var Worlds = connection.model('World', WorldSchema);
var Fortunes = connection.model('Fortune', FortuneSchema);

function mongooseRandomWorld(callback) {
  Worlds.findOne({
    id: h.randomTfbNumber()
  }).exec(callback);
}

function mongooseGetAllFortunes(callback) {
  Fortunes.find({})
    .exec(callback);
}

module.exports = {

  SingleQuery: function (req, res) {
    mongooseRandomWorld(function (err, result) {
      if (err) { return process.exit(1); }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(result));
    })
  },

  MultipleQueries: function (queries, req, res) {
    var queryFunctions = h.fillArray(mongooseRandomWorld, queries)

    async.parallel(queryFunctions, function (err, results) {
      if (err) { return process.exit(1); }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  },

  Fortunes: function (req, res) {
    mongooseGetAllFortunes(function (err, fortunes) {
      if (err) { return process.exit(1); }

      fortunes.push(h.ADDITIONAL_FORTUNE);
      fortunes.sort(function (a, b) {
        return a.message.localeCompare(b.message);
      });
      h.addTfbHeaders(res, 'html');
      res.end(h.fortunesTemplate({
        fortunes: fortunes
      }))
    });
  },

  Updates: function (queries, req, res) {
    var selectFunctions = h.fillArray(mongooseRandomWorld, queries);

    async.parallel(selectFunctions, function (err, worlds) {
      if (err) { return process.exit(1); }

      var updateFunctions = [];

      for (var i = 0; i < queries; i++) {
        (function (i) {
          updateFunctions.push(function (callback) {
            worlds[i].randomNumber = h.randomTfbNumber();
            Worlds.update({
              id: worlds[i].id
            }, {
              randomNumber: worlds[i].randomNumber
            }, callback);
          });
        })(i);
      }

      async.parallel(updateFunctions, function (err, results) {
        if (err) { return process.exit(1); }

        h.addTfbHeaders(res, 'json');
        // results does not have updated document information
        // if no err: all updates were successful
        res.end(JSON.stringify(worlds));
      });
    });
  }

};