// Connects to MongoDB using the mongoose driver
// Handles related routes

var h = require('../helper');
var Promise = require('bluebird');
// Can treat mongoose library as one that supports Promises
// these methods will then have "-Async" appended to them.
var Mongoose = Promise.promisifyAll(require('mongoose'));
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

function randomWorldPromise() {
  var id = h.randomTfbNumber();
  var promise = Worlds
    .findOneAsync({
      id: id
    })
    .then(function (world) {
      return world;
    })
    .catch(function (err) {
      process.exit(1);
    });
  return promise;
}

function promiseAllFortunes() {
  var promise = Fortunes
    .findAsync({})
    .then(function (fortunes) {
      return fortunes;
    })
    .catch(function (err) {
      process.exit(1);
    });
  return promise;
}

function updateWorld(world) {
  var promise = Worlds
    .updateAsync({
      id: world.randomNumber
    }, {
      randomNumber: world.randomNumber
    })
    .then(function (result) {
      return world;
    })
    .catch(function (err) {
      process.exit(1);
    });
  return promise;
}

module.exports = {

  SingleQuery: function(req, reply) {
    randomWorldPromise()
      .then(function (world) {
        reply(world)
          .header('Server', 'hapi');
      });
  },

  MultipleQueries: function(req, reply) {
    var queries = h.getQueries(req);
    var worldPromises = h.fillArray(randomWorldPromise(), queries);

    Promise
      .all(worldPromises)
      .then(function (worlds) {
        reply(worlds)
          .header('Server', 'hapi');
      });
  },

  Fortunes: function(req, reply) {
    promiseAllFortunes()
      .then(function (fortunes) {
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
    var worldPromises = [];

    for (var i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise());
    }

    Promise
      .all(worldPromises)
      .map(function (world) {
        world.randomNumber = h.randomTfbNumber();
        return updateWorld(world);
      })
      .then(function (worlds) {
        reply(worlds)
          .header('Server', 'hapi');
      });
  }

};
