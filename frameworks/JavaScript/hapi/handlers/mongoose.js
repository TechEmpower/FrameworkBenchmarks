// Connects to MongoDB using the mongoose driver
// Handles related routes

var h = require('../helper');
var Mongoose = require('mongoose');
var async = require('async');
var connection = Mongoose.connect('mongodb://127.0.0.1/hello_world');

var WorldSchema = new Mongoose.Schema({
    id :          Number,
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

  SingleQuery: function(req, reply) {
    mongooseRandomWorld(function (err, world) {
      if (err) { return process.exit(1); }

      reply(world)
        .header('Server', 'hapi');
    });
  },

  MultipleQueries: function(req, reply) {
    var queries = h.getQueries(req);
    var worldsToGet = h.fillArray(mongooseRandomWorld, queries);

    async.parallel(worldsToGet, function (err, worlds) {
      if (err) { return process.exit(1); }

      reply(worlds)
        .header('Server', 'hapi');
    });
  },

  Fortunes: function(req, reply) {
    mongooseGetAllFortunes(function (err, fortunes) {
      if (err) { return process.exit(1); }

      fortunes.push(h.ADDITIONAL_FORTUNE);
      fortunes.sort(function (a, b) {
        return a.message.localeCompare(b.message);
      });
      
      reply.view('fortunes', {
        fortunes: fortunes
      })
        .header('Content-Type', 'text/html')
        .header('Server', 'hapi');
    });
  },

  Updates: function(req, reply) {
    var queries = h.getQueries(req);
    var worldsToGet = h.fillArray(mongooseRandomWorld, queries);

    async.parallel(worldsToGet, function (err, worlds) {
      if (err) { return process.exit(1); }

      var updateFunctions = []

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
        }(i));
      }

      async.parallel(updateFunctions, function (err, results) {
        if (err) { return process.exit(1); }

        reply(worlds)
          .header('Server', 'hapi');
      });
    });
  }

};
