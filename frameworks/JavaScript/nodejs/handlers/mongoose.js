const h = require('../helper');
const async = require('async');
const Mongoose = require('mongoose');
const connection = Mongoose.createConnection('mongodb://tfb-database/hello_world');

// Mongoose Setup
const WorldSchema = new Mongoose.Schema({
  _id: Number,
  randomNumber: Number
}, {
  collection: 'world'
});
const FortuneSchema = new Mongoose.Schema({
  _id: Number,
  message: String
}, {
  collection: 'fortune'
});

const Worlds = connection.model('World', WorldSchema);
const Fortunes = connection.model('Fortune', FortuneSchema);

const mongooseRandomWorld = async () => {
  return await Worlds.findOne({
    _id: h.randomTfbNumber()
  }).lean().exec();
};

const mongooseGetAllFortunes = async () => {
  return await Fortunes.find({})
      .lean().exec();
};

const mongooseUpdateQuery = async () => {
  const world = await Worlds.findOne({_id: h.randomTfbNumber()}).lean().exec();
  world.randomNumber = h.randomTfbNumber();
  await Worlds.updateOne({_id: world._id}, {
    $set: {
      randomNumber: world.randomNumber
    }
  });
  return world;
};

module.exports = {

  SingleQuery: async (req, res) => {
    const result = await mongooseRandomWorld();
    h.addTfbHeaders(res, 'json');
    res.end(JSON.stringify(result));
  },

  MultipleQueries: async (queryCount, req, res) => {
    const queryFunctions = h.fillArray(mongooseRandomWorld, queryCount);
    const results = await Promise.all(queryFunctions);

    h.addTfbHeaders(res, 'json');
    res.end(JSON.stringify(results));
  },

  Fortunes: async (req, res) => {
    const fortunes = await mongooseGetAllFortunes();
    fortunes.push(h.additionalFortune());
    fortunes.sort((a, b) => {
      return a.message.localeCompare(b.message);
    });
    h.addTfbHeaders(res, 'html');
    res.end(h.fortunesTemplate({
      fortunes: fortunes
    }));
  },

  Updates: async (queryCount, req, res) => {
    const queryFunctions = h.fillArray(mongooseUpdateQuery, queryCount);
    const results = await Promise.all(queryFunctions);

    h.addTfbHeaders(res, 'json');
    res.end(JSON.stringify(results));
  }

};
