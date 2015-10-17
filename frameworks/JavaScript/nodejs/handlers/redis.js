var h = require('../helper');
var async = require('async');
// "If hiredis [pure C library] is installed, node_redis will use it by default.
// Otherwise, a pure JavaScript parser will be used."
// >> hiredis is installed for these tests
var redis = require('redis');
var client = redis.createClient(6379, '127.0.0.1', {});

client.on('error', function (err) {
  console.log('Redis Error: ' + err);
  // Do nothing further if Redis errors/is unavailable
});

function redisWorldId(id) {
  return 'world:' + id;
}

function redisRandomWorld(callback) {
  var id = h.randomTfbNumber();
  var redisId = redisWorldId(id);
  client.get(redisId, function (err, worldValue) {
    var world = {
      id: id,
      randomNumber: worldValue
    }
    callback(err, world);
  });
}

function redisSetWorld(world, callback) {
  var redisId = redisWorldId(world.id);
  client.set(redisId, world.randomNumber, function (err, result) {
    callback(err, world);
  });
}

function redisGetAllFortunes(callback) {
  client.lrange('fortunes', 0, -1, function (err, fortuneMessages) {
    if (err) { return process.exit(1); }

    var fortunes = fortuneMessages.map(function (e, i) {
      return { id: i + 1, message: e }
    });

    callback(err, fortunes)
  });
}


module.exports = {
  
  SingleQuery: function(req, res) {
    redisRandomWorld(function (err, world) {
      if (err) { return process.exit(1); }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(world));
    })
  },

  MultipleQueries: function(queries, req, res) {
    var queryFunctions = h.fillArray(redisRandomWorld, queries);

    async.parallel(queryFunctions, function (err, worlds) {
      if (err) { return process.exit(1); }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(worlds));
    })
  },

  Fortunes: function(req, res) {
    redisGetAllFortunes(function (err, fortunes) {
      if (err) { return process.exit(1); }

      h.addTfbHeaders(res, 'html');
      fortunes.push(h.ADDITIONAL_FORTUNE);
      fortunes.sort(function (a, b) {
        return a.message.localeCompare(b.message);
      });
      res.end(h.fortunesTemplate({
        fortunes: fortunes
      }));
    });
  },

  Updates: function(queries, req, res) {
    var getFunctions = h.fillArray(redisRandomWorld, queries);

    async.parallel(getFunctions, function (err, worlds) {
      if (err) { return process.exit(1); }

      var updateFunctions = [];

      worlds.forEach(function (w) {
        w.id = h.randomTfbNumber();
        updateFunctions.push(function (callback) {
          if (err) { return process.exit(1); }

          return redisSetWorld(w, callback);
        });
      });

      async.parallel(updateFunctions, function (err, updated) {
        if (err) { return process.exit(1); }

        h.addTfbHeaders(res, 'json');
        res.end(JSON.stringify(updated));
      });
    });

  }

};