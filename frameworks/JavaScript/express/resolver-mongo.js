const mongoose = require('mongoose');
const helper = require('./helper');

const WorldSchema = new mongoose.Schema({
  _id: Number,
  randomNumber: Number
}, {
  collection: 'world'
});
const World = mongoose.model('world', WorldSchema);

const FortuneSchema = new mongoose.Schema({
  _id: Number,
  message: String
}, {
  collection: 'fortune'
});
const Fortune = mongoose.model('fortune', FortuneSchema);

const toClientWorld = (world) => {
  if (world) {
    world.id = world._id;
    delete world._id;
  }
  return world;
};

async function getRandomWorld() {
  return toClientWorld(await World.findOne({_id: helper.randomizeNum()}).lean().exec());
}

// Methods

function arrayOfRandomWorlds(totalWorldsToReturn) {
  const totalIterations = helper.sanititizeTotal(totalWorldsToReturn);
  const promises = new Array(totalIterations);

  for (let i = 1; i <= totalIterations; i++) {
    promises[i - 1] = getRandomWorld();
  }

  return Promise.all(promises);
}

async function getAndUpdateRandomWorld() {
  // it would be nice to use findOneAndUpdate here, but for some reason the test fails with it.
  const world = await World.findOne({_id: helper.randomizeNum()}).lean().exec();
  world.randomNumber = helper.randomizeNum();
  await World.updateOne({
    _id: world._id
  }, {
    $set: {
      randomNumber: world.randomNumber
    }
  }).exec();
  return toClientWorld(world);
}

function updateRandomWorlds(totalToUpdate) {
  const totalIterations = helper.sanititizeTotal(totalToUpdate);
  const promises = new Array(totalIterations);

  for (let i = 1; i <= totalIterations; i++) {
    promises[i - 1] = getAndUpdateRandomWorld();
  }

  return Promise.all(promises);
}

const sayHello = () => {
  return JSON.stringify({
    message: "Hello, World!"
  });
};

module.exports = {
  Query: {
    helloWorld: () => sayHello(),
    getAllWorlds: async () => toClientWorld(await World.find({}).lean().exec()),
    singleDatabaseQuery: async () => toClientWorld(await World.findOne({_id: helper.randomizeNum()}).lean().exec()),
    multipleDatabaseQueries: async (parent, args) => await arrayOfRandomWorlds(args.total),
    getWorldById: async (parent, args) => toClientWorld(await World.findById(args.id).lean().exec()),
    getAllFortunes: async () => toClientWorld(await Fortune.find({}).lean().exec()),
    getRandomAndUpdate: (parent, args) => updateRandomWorlds(args.total)
  },
  Mutation: {
    createWorld: (parent, args) => {
      return World.create({_id: null, randomNumber: helper.randomizeNum()});
    },
    updateWorld: (parent, args) => {
      return World.updateOne({_id: args.id}, {
        randomNumber: args.randomNumber
      }).exec();
    }
  }
}