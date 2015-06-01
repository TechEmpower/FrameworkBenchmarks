var h = require('../helper');
var async = require('async');
// "If hiredis is installed, node_redis will use it by default.
// Otherwise, a pure JavaScript parser will be used."
// >> hiredis is installed for these tests
var redis = require('redis');
var client = redis.createClient();

client.on('error', function (err) {
  console.log('Redis Error: ' + err);
});

function redisWorldId(id) {
  return 'world:' + id;
}

function redisRandomWorld(callback) {
  var id = h.randomTfbNumber();
  var redisId = redisWorldId(id);
  client.get(redisId, function (err, worldValue) {
    if (err) { throw err; }
    callback(err, {
      id: id,
      randomNumber: worldValue
    })
  });
}

function redisSetWorld(world, callback) {
  var redisId = redisWorldId(world.id);
  client.set(redisId, world.randomNumber, function (err, result) {
    if (err) { throw err; }
    callback(err, world);
  });
}

function redisGetAllFortunes(callback) {
  client.lrange('fortunes', 0, -1, function (err, fortuneMessages) {
    if (err) { throw err; }

    var fortunes = fortuneMessages.map(function (e, i) {
      return { id: i + 1, message: e }
    });

    callback(err, fortunes)
  });
}


module.exports = {
  
  SingleQuery: function(req, res) {
    redisRandomWorld(function (err, world) {
      if (err) { throw err; }
      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(world));
    })
  },

  MultipleQueries: function(queries, req, res) {
    var queryFunctions = h.fillArray(redisRandomWorld, queries);

    async.parallel(queryFunctions, function (err, worlds) {
      if (err) { throw err; }
      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(worlds));
    })
  },

  Fortunes: function(req, res) {
    redisGetAllFortunes(function (err, fortunes) {
      if (err) { throw err; }
      h.addTfbHeaders(res, 'html');
      fortunes.push(h.ADDITIONAL_FORTUNE);
      fortunes.sort(function (a, b) {
        return a.message.localeCompare(b.message);
      })
      res.end(h.fortunesTemplate({
        fortunes: fortunes
      }));
    });
  },

  Updates: function(queries, req, res) {
    var getFunctions = h.fillArray(redisRandomWorld, queries);

    async.parallel(getFunctions, function (err, worlds) {
      if (err) { throw err; }
      var updateFunctions = [];

      worlds.forEach(function (w) {
        w.id = h.randomTfbNumber();
        updateFunctions.push(function (callback) {
          if (err) { throw err; }
          return redisSetWorld(w, callback);
        });
      });

      async.parallel(updateFunctions, function (err, updated) {
        if (err) { throw err; }
        h.addTfbHeaders(res, 'json');
        res.end(JSON.stringify(updated));
      });
    });

  }

};