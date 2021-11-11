const h = require('../helper');
const async = require('async');
const MongoClient = require('mongodb').MongoClient;
const collections = {
  World: null,
  Fortune: null
};

const mongoUrl = 'mongodb://tfb-database:27017';
const dbName = 'hello_world';

MongoClient.connect(mongoUrl, (err, database) => {
  // do nothing if there is err connecting to db

  collections.World = database.db(dbName).collection('world');
  collections.Fortune = database.db(dbName).collection('fortune');
});


const mongodbRandomWorld = (callback) => {
  collections.World.findOne({
    id: h.randomTfbNumber()
  }, (err, world) => {
    world._id = undefined; // remove _id from query response
    callback(err, world);
  });
};

const mongodbGetAllFortunes = (callback) => {
  collections.Fortune.find().toArray((err, fortunes) => {
    callback(err, fortunes);
  })
};

const mongodbDriverUpdateQuery = (callback) => {
  collections.World.findOne({ id: h.randomTfbNumber() }, (err, world) => {
    world.randomNumber = h.randomTfbNumber();
    collections.World.update({ id: world.id }, world, (err, updated) => {
      callback(err, { id: world.id, randomNumber: world.randomNumber });
    });
  });
};


module.exports = {

  SingleQuery: (req, res) => {
    mongodbRandomWorld((err, result) => {
      if (err) { return process.exit(1) }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(result));
    });
  },

  MultipleQueries: (queries, req, res) => {
    const queryFunctions = h.fillArray(mongodbRandomWorld, queries);

    async.parallel(queryFunctions, (err, results) => {
      if (err) { return process.exit(1) }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  },

  Fortunes: (req, res) => {
    mongodbGetAllFortunes((err, fortunes) => {
      if (err) { return process.exit(1) }

      fortunes.push(h.additionalFortune());
      fortunes.sort(function (a, b) {
        return a.message.localeCompare(b.message);
      });
      h.addTfbHeaders(res, 'html');
      res.end(h.fortunesTemplate({
        fortunes: fortunes
      }));
    });
  },

  Updates: (queries, req, res) => {
    const queryFunctions = h.fillArray(mongodbDriverUpdateQuery, queries);

    async.parallel(queryFunctions, (err, results) => {
      if (err) { return process.exit(1) }

      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(results));
    });
  }

};
