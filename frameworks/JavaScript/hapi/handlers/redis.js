// Connects to Redis using the node_redis and hiredis drivers
// Handles related routes

// "If hiredis [pure C library] is installed, node_redis will use it by default.
// Otherwise, a pure JavaScript parser will be used."
// >> hiredis is installed for these tests

var h = require('../helper');
var Promise = require('bluebird');
// Can treat redis library as one that supports Promises
// these methods will then have "-Async" appended to them.
var redis = Promise.promisifyAll(require('redis'));
var client = redis.createClient(6379, '127.0.0.1', {});

client.on('error', function (err) {
  console.log('Redis Error: ' + err);
  // Do nothing further if Redis errors/is unavailable
});

function redisWorldId(id) {
  return 'world:' + id;
}

function randomWorldPromise() {
  var id = h.randomTfbNumber();
  var redisId = redisWorldId(id);

  var promise = client.getAsync(redisId)
    .then(function (worldValue) {
      return {
        id: id,
        randomNumber: worldValue
      }
    })
    .catch(function (err) {
      process.exit(1);
    });
  return promise;
}

function redisSetWorld(world) {
  var redisId = redisWorldId(world.id);
  var promise = client
    .setAsync(redisId, world.randomNumber)
    .then(function (result) {
      return world;
    })
    .catch(function (err) {
      process.exit(1);
    });
  return promise;
}

function redisGetAllFortunes() {
  var promise = client
    .lrangeAsync('fortunes', 0, -1)
    .then(function (fortuneMessages) {
      var fortunes = fortuneMessages.map(function (e, i) {
        return { id: i + 1, message: e }
      });
      return fortunes;
    })
    .catch(function (err) {
      if (err) { return process.exit(1); }
    });
  return promise;
}


module.exports = {
  
  SingleQuery: function(req, reply) {
    randomWorldPromise()
      .then(function (world) {
        reply(world)
          .header('Server', 'hapi');
      })
      .catch(function (err) {
        if (err) { return process.exit(1); }
      })
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
    redisGetAllFortunes()
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
      })
      .catch(function (err) {
        process.exit(1);
      })
  },

  Updates: function(req, reply) {
    var queries = h.getQueries(req)
    var worldPromises = h.fillArray(randomWorldPromise(), queries);

    Promise
      .all(worldPromises)
      .map(function (world) {
        world.randomNumber = h.randomTfbNumber();
        return redisSetWorld(world);
      })
      .then(function (updated) {
        reply(updated)
          .header('Server', 'hapi');
      })
      .catch(function (err) {
        process.exit(1);
      });
  }

};