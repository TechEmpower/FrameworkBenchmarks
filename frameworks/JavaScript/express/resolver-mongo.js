const mongoose = require('mongoose');

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

const randomizeNum = () => {

    return Math.floor(Math.random() * 10000) + 1;
};

async function arrayOfRandomWorlds(totalWorldToReturn) {

    var totalIterations = sanititizeTotal(totalWorldToReturn);
    var arr = [];

    return new Promise(async (resolve, reject) => {
        for(var i = 0; i < totalIterations; i++) {
            arr.push(await World.findOne({ id: randomizeNum() }));
        }
        if(arr.length == totalIterations) {
            resolve(arr);
        }
    });
};

async function updateRandomWorlds(totalToUpdate) {

    const totalIterations = sanititizeTotal(totalToUpdate);
    var arr = [];

    return new Promise(async (resolve, reject) => {
        for(var i = 0; i < totalIterations; i++) {

            arr.push(await World.findOneAndUpdate({ id: randomizeNum() }, { randomNumber: randomizeNum() }));
        }
        if(arr.length == totalIterations) {
            resolve(arr);
        }
    });
};

const sanititizeTotal = (total) => {

    var totalIterations;

    if (!total) {
        totalIterations = 1;
    } else if(total < 501 && total > 0) {
        totalIterations = total;
    } else if (total > 500) {
        totalIterations = 500;
    } else {
        totalIterations = 1;
    }
    return totalIterations;
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
        singleDatabaseQuery: async() => await World.findOne({ id: randomizeNum() }),
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