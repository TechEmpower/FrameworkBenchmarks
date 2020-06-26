const mongoose = require('mongoose');
const helper = require('./helper');

const Schema = mongoose.Schema,
  ObjectId = Schema.ObjectId;

const WorldSchema = new mongoose.Schema({
  id: Number,
  randomNumber: Number
}, {
    collection: 'world'
  }),
  World = mongoose.model('world', WorldSchema);

const FortuneSchema = new mongoose.Schema({
  id: Number,
  message: String
}, {
    collection: 'fortune'
  }),
  Fortune = mongoose.model('fortune', FortuneSchema);

// Methods

async function arrayOfRandomWorlds(totalWorldToReturn) {

    var totalIterations = helper.sanititizeTotal(totalWorldToReturn);
    var arr = [];

    return new Promise(async (resolve, reject) => {
        for(var i = 0; i < totalIterations; i++) {
            arr.push(await World.findOne({ id: helper.randomizeNum() }));
        }
        if(arr.length == totalIterations) {
            resolve(arr);
        }
    });
};

async function updateRandomWorlds(totalToUpdate) {

    const totalIterations = helper.sanititizeTotal(totalToUpdate);
    var arr = [];

    return new Promise(async (resolve, reject) => {
        for(var i = 0; i < totalIterations; i++) {

            arr.push(await World.findOneAndUpdate({ id: helper.randomizeNum() }, { randomNumber: helper.randomizeNum() }));
        }
        if(arr.length == totalIterations) {
            resolve(arr);
        }
    });
};

const sayHello = () => {

    var helloWorld = new Object;
    helloWorld.message = "Hello, World!";

    return JSON.stringify(helloWorld);
};

module.exports = {
    Query: {
        helloWorld: () => sayHello(),
        getAllWorlds: async() => await World.find({}),
        singleDatabaseQuery: async() => await World.findOne({ id: helper.randomizeNum() }),
        multipleDatabaseQueries: async(parent, args) => await arrayOfRandomWorlds(args.total),
        getWorldById: async(parent, args) => await World.findById(args.id),
        getAllFortunes: async() => await Fortune.find({}),
        getRandomAndUpdate: async(parent, args) => await updateRandomWorlds(args.total)
    },
    Mutation: {
        createWorld: async(parent, args) => {
            let randInt = Math.floor(Math.random() * 1000) + 1;
            return await World.create({ id: null, randomNumber: randInt });
        },
        updateWorld: async(parent, args) => {
            return await World.update({id: args.id, randomNumber: args.randomNumber});
        }
    }
}