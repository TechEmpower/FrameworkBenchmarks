/**
 * RedisController
 *
 * @description :: Connects to Redis using the node_redis and hiredis drivers
 *   Handles redis routes
 *   "If hiredis [pure C library] is installed, node_redis will use it by default.
 *   Otherwise, a pure JavaScript parser will be used."
 *   >> hiredis is installed for these tests
 */

var h = require('../services/helper')
var Promise = require('bluebird')
// Can treat redis library as one that supports Promises
// these methods will then have "-Async" appended to them.
var redis = Promise.promisifyAll(require('redis'))
var client = redis.createClient()

client.on('error', function (err) {
  console.log('Redis Error: ' + err)
  // Do nothing further if Redis errors/is unavailable
});

function redisWorldId(id) {
  return 'world:' + id
}

function randomWorldPromise() {
  var id = h.randomTfbNumber()
  var redisId = redisWorldId(id)

  var promise = client.getAsync(redisId)
    .then(function (worldValue) {
      return {
        id: id,
        randomNumber: worldValue
      }
    })
    .catch(function (err) {
      console.log(err.stack)
    })
  return promise
}

function redisSetWorld(world) {
  var redisId = redisWorldId(world.id)
  var promise = client
    .setAsync(redisId, world.randomNumber)
    .then(function (result) {
      return world
    })
    .catch(function (err) {
      console.log(err.stack)
    })
  return promise
}

function redisGetAllFortunes() {
  var promise = client
    .lrangeAsync('fortunes', 0, -1)
    .then(function (fortuneMessages) {
      var fortunes = fortuneMessages.map(function (e, i) {
        return { id: i + 1, message: e }
      })
      return fortunes;
    })
    .catch(function (err) {
      console.log(err.stack)
    })
  return promise
}


module.exports = {
  
  Single: function(req, res) {
    randomWorldPromise()
      .then(function (world) {
        res.json(world)
      })
      .catch(function (err) {
        console.log(err.stack)
      })
  },

  Multiple: function(req, res) {
    var queries = h.getQueries(req)
    var worldPromises = []

    for (var i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise())
    }

    Promise
      .all(worldPromises)
      .then(function (worlds) {
         res.json(worlds)
      });
  },

  Fortunes: function(req, res) {
    redisGetAllFortunes()
      .then(function (fortunes) {
        fortunes.push(h.ADDITIONAL_FORTUNE)
        fortunes.sort(function (a, b) {
          return a.message.localeCompare(b.message)
        })
        res.render('fortunes', { fortunes: fortunes })
      })
      .catch(function (err) {
        console.log(err.stack)
      })
  },

  Updates: function(req, res) {
    var queries = h.getQueries(req)
    var worldPromises = []

    for (var i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise())
    }

    Promise
      .all(worldPromises)
      .map(function (world) {
        world.randomNumber = h.randomTfbNumber()
        return redisSetWorld(world)
      })
      .then(function (updated) {
        res.json(updated)
      })
      .catch(function (err) {
        console.log(err.stack)
      })
  }

};