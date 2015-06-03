var h = require('../helper');
var async = require('async');
var MongoClient = require('mongodb').MongoClient;
var collections = {
  World: null,
  Fortune: null
};
MongoClient.connect('mongodb://127.0.0.1/hello_world?maxPoolSize=5', function (err, db) {
  // do nothing if there is err connecting to db

  collections.World = db.collection('world');
  collections.Fortune = db.collection('fortune');
});


function mongodbRandomWorld(callback) {
  collections.World.findOne({
    id: h.randomTfbNumber()
  }, function (err, world) {
    world._id = undefined; // remove _id from query response
    callback(err, world);
  });
}

function mongodbGetAllFortunes(callback) {
  collections.Fortune.find().toArray(function (err, fortunes) {
    callback(err, fortunes);
  })
}

function mongodbDriverUpdateQuery(callback) {
  collections.World.findAndModify({
    id: h.randomTfbNumber()
  }, [['_id','asc']], {
    $set: {randomNumber: h.randomTfbNumber()}
  }, {}, function (err, world) {
    world.value._id = undefined; // remove _id from query response
    callback(err, world.value);
  });
}


module.exports = {

  SingleQuery: function (req, res) {
    mongodbRandomWorld(function (err, result) {
      if (err) { return process.exit(1) }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(result));
    });
  },

  MultipleQueries: function (queries, req, res) {
    var queryFunctions = h.fillArray(mongodbRandomWorld, queries);

    async.parallel(queryFunctions, function (err, results) {
      if (err) { return process.exit(1) }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  },

  Fortunes: function (req, res) {
    mongodbGetAllFortunes(function (err, fortunes) {
      if (err) { return process.exit(1) }

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
    var queryFunctions = h.fillArray(mongodbDriverUpdateQuery, queries);

    async.parallel(queryFunctions, function (err, results) {
      if (err) { return process.exit(1) }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  }

};