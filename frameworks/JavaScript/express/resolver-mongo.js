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

async function arrayOfRandomWorlds(totalWorldsToReturn) {
  const totalIterations = helper.sanititizeTotal(totalWorldsToReturn);
  const promises = [];

  for (let i = 1; i <= totalIterations; i++) {
    promises.push(getRandomWorld());
  }

  return await Promise.all(promises);
}

async function getAndUpdateRandomWorld() {
  return toClientWorld(await World.findOneAndUpdate({_id: helper.randomizeNum()}, {
    randomNumber: helper.randomizeNum()
  }, {
    new: true
  }).lean().exec());
}

async function updateRandomWorlds(totalToUpdate) {
  const totalIterations = helper.sanititizeTotal(totalToUpdate);
  const promises = [];

  for (let i = 1; i <= totalIterations; i++) {
    promises.push(getAndUpdateRandomWorld());
  }

  return await Promise.all(promises);
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
    getRandomAndUpdate: async (parent, args) => await updateRandomWorlds(args.total)
  },
  Mutation: {
    createWorld: async (parent, args) => {
      let randInt = Math.floor(Math.random() * 1000) + 1;
      return await World.create({_id: null, randomNumber: randInt});
    },
    updateWorld: async (parent, args) => {
      return await World.updateOne({_id: args.id}, {
        randomNumber: args.randomNumber
      });
    }
  }
}