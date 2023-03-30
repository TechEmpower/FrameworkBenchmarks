const h = require('../helper');
const Mongoose = require('mongoose');
// These .set() calls can be removed when mongoose is upgraded to v5.
Mongoose.set('useNewUrlParser', true);
Mongoose.set('useFindAndModify', false);
Mongoose.set('useUnifiedTopology', true);
const connection = Mongoose.createConnection('mongodb://tfb-database/hello_world');

/**
 * Note! The benchmarks say we should use "id" as a property name.
 * However, Mongo provides a default index on "_id", so to be equivalent to the other tests, we use
 * the same, default index provided by the database.
 *
 */

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

const toClientWorld = (world) => {
  if (world) {
    world.id = world._id;
    delete world._id;
  }
  return world;
};

const mongooseRandomWorld = async () => {
  return toClientWorld(await Worlds.findOne({
    _id: h.randomTfbNumber()
  }).lean().exec());
};

const mongooseGetAllFortunes = async () => {
  return (await Fortunes.find({})
      .lean().exec()).map(toClientWorld);
};

async function getUpdateRandomWorld() {
  // it would be nice to use findOneAndUpdate here, but for some reason the test fails with it.
  const world = await Worlds.findOne({_id: h.randomTfbNumber()}).lean().exec();
  world.randomNumber = h.randomTfbNumber();
  await Worlds.updateOne({
    _id: world._id
  }, {
    $set: {
      randomNumber: world.randomNumber
    }
  }).exec();
  return toClientWorld(world);
}

module.exports = {

  SingleQuery: async (req, res) => {
    const result = await mongooseRandomWorld();
    h.addTfbHeaders(res, 'json');
    res.end(JSON.stringify(result));
  },

  MultipleQueries: async (queryCount, req, res) => {
    const queryFunctions = [];
    for (let i = 0; i < queryCount; i++) {
      queryFunctions.push(mongooseRandomWorld());
    }
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
    res.end(h.fortunesTemplate({fortunes}));
  },

  Updates: async (queryCount, req, res) => {
    const promises = [];

    for (let i = 1; i <= queryCount; i++) {
      promises.push(getUpdateRandomWorld());
    }

    h.addTfbHeaders(res, 'json');
    res.end(JSON.stringify(await Promise.all(promises)));
  }

};
